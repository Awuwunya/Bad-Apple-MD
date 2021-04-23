using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Runtime.ConstrainedExecution;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Converter {
	class Tile {
		// hlhlhlhl
		public ushort d;

		public Tile(ushort d) {
			this.d = d;
		}
	}

	class TileDelta {
		public ushort vram;
		public ushort data;

		public TileDelta(ushort d, ushort vram) {
			data = d;
			this.vram = vram;
		}
	}

	class TileList {
		public ushort vram;
		public int len;
		public byte[] data;

		public TileList(byte[] d, int len, ushort vram) {
			data = d;
			this.len = len;
			this.vram = vram;
		}
	}

	class Video {
		static int[] PALDELTAS = { 0x700, 0xD80, 0x1B00 };
		static int[] MAXDELTAS = { 0x5B4, 0xB00, 0x14E0 };
		const int DISTANCE = 2, LOOKUP = 2;
		const int DIV = 1, W = 320, H = 224;
		static int THREADS = Environment.ProcessorCount, SAVETH = THREADS * 4, ROWS = W / 8 * H, realframes = 0;
		static string[] files;
		static List<TileList>[] result;
		static Tile[][] plane;
		static List<TileDelta>[] deltas;
		static List<int> totals;

		static void Main(string[] args) {
			Stopwatch sw = new Stopwatch();
			sw.Start();
			Console.WriteLine("Start video encode with " + THREADS + " threads!");

			files = Directory.GetFiles(@"delta\img");
			Thread[] threads = new Thread[THREADS * 4];
			saves = new byte[][][] { new byte[files.Length][], new byte[files.Length][], new byte[files.Length][], };
			plane = new Tile[files.Length][];
			deltas = new List<TileDelta>[files.Length];
			result = new List<TileList>[files.Length];
			totals = new List<int>(files.Length / 2);

			Run(threads, SAVETH, (string i, int off) => executeFiles(int.Parse(i), off));
			getDeltas();
			saveSmall();
			Run(threads, SAVETH, (string i, int off) => save(int.Parse(i), off));
			saveBig();
			//	Run(threads, THREADS, (string i, int off) => comp(int.Parse(i), off));

			File.WriteAllText(@"delta\files.mac", "files = " + realframes +"\nrealfiles = "+ files.Length +"\nhres = "+ W);

			Console.WriteLine("build the ROM");
			cmp("\"build.bat\"", new string[] { });
			sw.Stop();
			Console.WriteLine("done in "+ (sw.ElapsedMilliseconds / 1000f) +"s");
			Console.ReadKey();
		}

		private static void optimize(int ti, int off) {
			Stopwatch sw = new Stopwatch();
			sw.Restart();
			int i = ti;
			int misses = 0;

			for (; i < files.Length / DIV; i += off) {
				result[i] = new List<TileList>(128);
				List<TileDelta> prg = new List<TileDelta>();

				foreach(TileDelta x in deltas[i].OrderBy((TileDelta t) => t.vram)) {
					if(prg.Count == 0) {
						prg.Add(x);
						continue;
					}

					// check how to combine effectively
					int diff = x.vram - prg.Count - prg[0].vram;

					if (diff < DISTANCE) {
						if(prg.Count + diff >= 0x100) {
							// split data
							result[i].Add(getPrg(prg));
							prg = new List<TileDelta>();
							misses++;

						} else {
							// add rows that were missing
							for (int v = prg.Count + prg[0].vram; v < x.vram; v++)
								prg.Add(new TileDelta(plane[i][v].d, (ushort) (v)));
						}

						prg.Add(x);

					} else {
						// create a new combination
						result[i].Add(getPrg(prg));
						prg = new List<TileDelta>() { x };
					}
				}

				if(prg.Count > 0) result[i].Add(getPrg(prg));
			}

			sw.Stop();
			Console.WriteLine(sw.ElapsedMilliseconds + "ms optimize " + ti.ToString("X2") +" misses "+ misses);
		}

		static object _lock = new object();
		static int[] fskiptbl =		{ 0, 0, 0, 0, 0, 0, 0, 0,  1, 1, 1, 1, 1,  0, 0, 0, 0, 0,  2, 2,  1, 1, 1, 1,  2, 2, 2, 2, 2,  0, 1, 2, };
		static int[] exdifftbl =	{ 0, 0, 0, 0, 0, 0, 0, 0,  0, 0, 0, 0, 0,  0, 0, 3, 3, 3,  0, 0,  3, 3, 3, 3,  0, 3, 3, 3, 3,  2, 2, 2, };
		static int[] lnstbl =		{ 2, 2, 2, 2, 3, 3, 3, 3,  2, 2, 2, 3, 3,  4, 4, 4, 5, 5,  2, 2,  4, 4, 5, 5,  3, 3, 4, 4, 5,  5, 5, 7, };
		static int[] simtbl =		{ 0, 1, 2, 3, 0, 1, 2, 3,  0, 1, 3, 2, 3,  1, 2, 3, 2, 3,  1, 3,  2, 3, 2, 3,  2, 3, 2, 3, 2,  8, 8, 10, };

		private static void getDeltas() {
			Stopwatch sw = new Stopwatch();
			Tile[] screen = new Tile[ROWS];

			for (int z = 0; z < screen.Length; z++)
				screen[z] = new Tile(0);

			for (int i = 0; i < files.Length / DIV; i ++) {
				sw.Restart();
				Tile[] sc2 = new Tile[ROWS];

				int misses = 0, fskip = 0;
				int sim = 0, lns = 0, exdiff = 0;

				for (int y = 0;y < 32;y++) {
					// copy screen data
					for (int x = 0; x < ROWS; x++) 
						sc2[x] = new Tile(screen[x].d);

					deltas[i] = new List<TileDelta>(256);

					fskip = fskiptbl[y];
					exdiff = exdifftbl[y];
					lns = lnstbl[y];
					sim = simtbl[y];

					for (int x = 0; x < ROWS; x ++) {
						if (getDiff(screen[x], plane[i][x], exdiff) > (((x + i) % lns) != 0 ? sim : 0)) {
							// must add a new delta
							deltas[i].Add(new TileDelta(plane[i][x].d, (ushort) (x)));

						//	if (deltas[i].Count > MAXDELTAS) {
						//		goto failmax;
						//	}
						}
					}

					// optimize step
					result[realframes] = new List<TileList>(128);
					List<TileDelta> prg = new List<TileDelta>();
					int tiles = 0;

					foreach (TileDelta x in deltas[i]) {
						if (prg.Count == 0) {
							prg.Add(x);
							sc2[x.vram].d = x.data;
							continue;
						}

						// check how to combine effectively
						int diff = x.vram - prg.Count - prg[0].vram;

						if (diff < DISTANCE) {
							if (prg.Count + diff + 1 >= 0x100) {
								// split data
								result[realframes].Add(getPrg(prg));
								tiles += prg.Count + LOOKUP;
								prg = new List<TileDelta>();
								misses++;

							} else {
								// add rows that were missing
								for (int v = prg.Count + prg[0].vram; v < x.vram; v++) {
									prg.Add(new TileDelta(sc2[v].d = plane[i][v].d, (ushort) (v)));
								}
							}

							prg.Add(x);
							sc2[x.vram].d = x.data;

						} else {
							// create a new combination
							result[realframes].Add(getPrg(prg));
							tiles += prg.Count + LOOKUP;
							prg = new List<TileDelta>() { x };
							sc2[x.vram].d = x.data;
						}

						if (tiles > MAXDELTAS[fskip]) goto failmax;
					}

					if (prg.Count > 0) result[realframes].Add(getPrg(prg));
					tiles += prg.Count + LOOKUP;
					totals.Add(tiles);
					if (tiles > MAXDELTAS[fskip]) goto failmax;

					i += fskip;
					goto next;

					failmax:;
					if (prg.Count > 0) result[realframes].Add(getPrg(prg));
				}

				Console.WriteLine("FAIL " + sim + " " + lns + " "+ i);

				// copy new screen
				next: screen = sc2;
				realframes++;

				sw.Stop();
				if(sw.ElapsedMilliseconds > 60)
					Console.WriteLine(sw.ElapsedMilliseconds + "ms deltas " + i +" misses "+ misses);
			}
		}

		static TileList getPrg(List<TileDelta> p) {
			if (p.Count - 1 > 0xFF) {
				Console.WriteLine("fuckup");
				Console.ReadKey();
			}

			byte[] r = new byte[p.Count * 2];
			int x = 0;

			foreach (TileDelta d in p) {
				r[x++] = (byte)(d.data >> 8);
				r[x++] = (byte) (d.data & 0xFF);
			}

			return new TileList(r, p.Count - 1, p[0].vram);
		}

		static int getDiff(Tile a, Tile b, int highdiff) {
			if (a.d == b.d) return 0;
			int d = 0;

			if (highdiff == 0) {
				if ((a.d & 0xAAAA) != 0xAAAA || (b.d & 0xAAAA) != 0xAAAA)
					return 0xFFFF;

			/*		if ((a.d & 0x60) != (b.d & 0x60)) d++;
					if ((a.d & 0x30) != (b.d & 0x30)) d++;
					if ((a.d & 0x06) != (b.d & 0x06)) d++;
					if ((a.d & 0x03) != (b.d & 0x03)) d++;
					return d;*/

				highdiff = 0xFFFF;
			}

			// highdiff mode
			if ((a.d & 0xC000) != (b.d & 0xC000) && ((a.d & 0xC000) == 0 || (a.d & 0xC000) == 0x4000) && ((b.d & 0xC000) == 0 || (b.d & 0xC000) == 0x4000)) d += highdiff;
			if ((a.d & 0x0C00) != (b.d & 0x0C00) && ((a.d & 0x0C00) == 0 || (a.d & 0x0C00) == 0x0400) && ((b.d & 0x0C00) == 0 || (b.d & 0x0C00) == 0x0400)) d += highdiff;
			if ((a.d & 0x3000) != (b.d & 0x3000) && ((a.d & 0x3000) == 0 || (a.d & 0x3000) == 0x1000) && ((b.d & 0x3000) == 0 || (b.d & 0x3000) == 0x1000)) d += highdiff;
			if ((a.d & 0x0300) != (b.d & 0x0300) && ((a.d & 0x0300) == 0 || (a.d & 0x0300) == 0x0100) && ((b.d & 0x0300) == 0 || (b.d & 0x0300) == 0x0100)) d += highdiff;
			if ((a.d & 0x00C0) != (b.d & 0x00C0) && ((a.d & 0x00C0) == 0 || (a.d & 0x00C0) == 0x0040) && ((b.d & 0x00C0) == 0 || (b.d & 0x00C0) == 0x0040)) d += highdiff;
			if ((a.d & 0x000C) != (b.d & 0x000C) && ((a.d & 0x000C) == 0 || (a.d & 0x000C) == 0x0004) && ((b.d & 0x000C) == 0 || (b.d & 0x000C) == 0x0004)) d += highdiff;
			if ((a.d & 0x0030) != (b.d & 0x0030) && ((a.d & 0x0030) == 0 || (a.d & 0x0030) == 0x0010) && ((b.d & 0x0030) == 0 || (b.d & 0x0030) == 0x0010)) d += highdiff;
			if ((a.d & 0x0003) != (b.d & 0x0003) && ((a.d & 0x0003) == 0 || (a.d & 0x0003) == 0x0001) && ((b.d & 0x0003) == 0 || (b.d & 0x0003) == 0x0001)) d += highdiff;

			if ((a.d & 0xC000) != (b.d & 0xC000)) d++;
			if ((a.d & 0x3000) != (b.d & 0x3000)) d++;
			if ((a.d & 0x0C00) != (b.d & 0x0C00)) d++;
			if ((a.d & 0x0300) != (b.d & 0x0300)) d++;
			if ((a.d & 0x00C0) != (b.d & 0x00C0)) d++;
			if ((a.d & 0x0030) != (b.d & 0x0030)) d++;
			if ((a.d & 0x000C) != (b.d & 0x000C)) d++;
			if ((a.d & 0x0003) != (b.d & 0x0003)) d++;
			return d;
		}

		private static void executeFiles(int ti, int off) {
			Stopwatch sw = new Stopwatch();
			int i = ti;

			for (; i < files.Length / DIV; i += off) {
				sw.Restart();
				Bitmap b = (Bitmap) Image.FromFile(files[i]);
				BitmapData bd = b.LockBits(new Rectangle(0, 0, b.Width, b.Height), ImageLockMode.ReadWrite, b.PixelFormat);

				plane[i] = new Tile[ROWS];

				unsafe {
					byte* px = (byte*) bd.Scan0;
					int s = Image.GetPixelFormatSize(bd.PixelFormat) / 8;

					for (int y = 0; y < H; y ++) {
						for (int x = 0; x < W; x += 8) {
							ushort t = 0;

							for(int xx = 0;xx < 8;xx ++) {
								byte c = getColor(px[(y * bd.Stride) + ((x + xx) * s) + 1]);
								t |= (ushort) (c << ((7 - xx) << 1));
							}

							plane[i][(y & 7) + ((x / 8) * 8) + (y / 8 * W)] = new Tile(t);
						}
					}
				}

				b.UnlockBits(bd);
				b.Dispose();
				sw.Stop();
				if((i / off % 50) == 25)
					Console.WriteLine(sw.ElapsedMilliseconds + "ms load " + files[i]);
			}
		}

		static byte[] collut = { 0, 2, 3, 1 };

		static byte getColor(byte p) {
			return collut[(p / 75)];
		}

		private static void Run(Thread[] threads, int off, Action<string, int> p) {
			for (int i = 0; i < off; i++) {
				threads[i] = new Thread((x) => {
					p((string) x, off);
				}) {
					Priority = ThreadPriority.BelowNormal
				};

			}

			for (int i = 0; i < off; i++)
				threads[i].Start(i + "");

			for (int i = 0; i < off; i++)
				threads[i].Join();
		}

		static void cmp(string prg, string[] args) {
			try {
				Process.Start(new ProcessStartInfo(prg, string.Join(" ", args)) { CreateNoWindow = true, UseShellExecute = false }).WaitForExit();
			} catch(Exception e) {
				Console.WriteLine(e.Message);
			}
		}

	/*	static void comp(int ti, int off) {
			Stopwatch sw = new Stopwatch();
			int x = 0;

			for (int i = ti; i < files.Length / DIV; i += off) {
				sw.Restart();

				cmp("\"G:\\RESEARCH\\bad apple\\delta\\compcmp.exe\"", new string[] { "\"G:\\RESEARCH\\bad apple\\delta\\" + subs[0] + "\\" + i.ToString("X4") + ".dat\"", "\"G:\\RESEARCH\\bad apple\\delta\\" + subs[0] + " cmp\\" + i.ToString("X4") + ".dat\"" });
				cmp("\"G:\\RESEARCH\\bad apple\\delta\\compcmp.exe\"", new string[] { "\"G:\\RESEARCH\\bad apple\\delta\\" + subs[1] + "\\" + i.ToString("X4") + ".dat\"", "\"G:\\RESEARCH\\bad apple\\delta\\" + subs[1] + " cmp\\" + i.ToString("X4") + ".dat\"" });

				sw.Stop();
				if ((++x % 10) == 5)
					Console.WriteLine(sw.ElapsedMilliseconds + "ms cmp " + ti + " p " + i.ToString("X4"));
			}
		}*/

		// ti = 0:
		//    1 byte per data
		// ti = 1:
		//    4 bits per len
		// ti = 2:
		//    1 byte per low byte of VRAM
		// ti = 3:
		//    2 bytes per length, 4 lists (0-3) for different high nibbles for VRAM

		static void save(int ti, int off) {
			Stopwatch sw = new Stopwatch();

			for (int i = ti; i < realframes; i += off) {
				for(int z = 0;z < 3;z++) {
					// generate bytes
					sw.Restart();
					List<byte> bytes = new List<byte>(1024);

					foreach (TileList t in result[i]) {
						switch(z) {
							case 0: 
								bytes.AddRange(t.data);
								break;

							case 1:
								bytes.Add((byte) t.len);
								break;

							case 2:
								bytes.Add((byte) ((t.vram >> 6) & 0x3F));
								bytes.Add((byte) ((t.vram << 2) & 0xFF));
								break;
						}
					}

					// do rle encoding
					/*	List<byte> rle = new List<byte>(1024);

						int start = 0, last = 0;

						for(int x = 0;x < bytes.Count;x++) {
							// check length of rle
							for(int y = x + 1;y < bytes.Count;y ++) {
								if (bytes[x] == bytes[y]) last = y;
								else {
									break;
								}
							}

							// inspect length
							if(last - x >= RLE) {
								if(start < x) {
									// non-repeating block
									while(x - start > 0x80) {
										rle.Add(0x80);

										for(int s = 0;s < 0x80;s ++)
											rle.Add(bytes[start + s]);

										start += 0x80;
									}

									// add the last bit
									rle.Add((byte)-(x - start));

									for (; start < x; start++)
										rle.Add(bytes[start]);
								}

								// this will be RLE encoded
								while(last - x > 0x7F) {
									rle.Add(0x7F);
									rle.Add(bytes[x]);
									x += 0x7F;
								}

								// add the last bit
								rle.Add((byte) (last - x));
								rle.Add(bytes[x]);
								start = x = last;
							}
						}

						if (start < bytes.Count) {
							// non-repeating block
							while (bytes.Count - start > 0x80) {
								rle.Add(0x80);

								for (int s = 0; s < 0x80; s++)
									rle.Add(bytes[start + s]);

								start += 0x80;
							}

							// add the last bit
							rle.Add((byte) -(bytes.Count - start));

							for (; start < bytes.Count; start++)
								rle.Add(bytes[start]);
						}

						rle.Add(0);*/

					// flush bytes
					saves[z][i] = bytes.ToArray();
				}

				result[i].Clear();
				result[i] = null;

				sw.Stop();
			//	if ((i % 100) == 10)
			//		Console.WriteLine(sw.ElapsedMilliseconds + "ms save " + ti +" p "+ i);
			}
		}

		static byte[][][] saves;

		static void saveSmall() {
			Stopwatch sw = new Stopwatch();
			sw.Restart();
			// generate bytes
			List<byte> bytes = new List<byte>(4096);

			string fn = @"delta\list.dat";
			if (File.Exists(fn)) File.Delete(fn);

			for (int i = 0; i < realframes; i++) {
				int[] c = { -1, -1, -1, -1 };

				foreach (TileList t in result[i]) {
					c[t.vram >> 12]++;
				}

				bytes.Add((byte) (c[1] >> 8));
				bytes.Add((byte) (c[1] & 0xFF));
				bytes.Add((byte) (c[0] >> 8));
				bytes.Add((byte) (c[0] & 0xFF));
				bytes.Add((byte) (c[2] >> 8));
				bytes.Add((byte) (c[2] & 0xFF));

				if (c[3] != -1) {
					Console.WriteLine("error: " + string.Join(",", c));
				}
			}

			// flush bytes
			using (var f = File.OpenWrite(fn)) {
				f.Write(bytes.ToArray(), 0, bytes.Count);
				f.Flush();
				f.Close();
			}

			sw.Stop();
			Console.WriteLine(sw.ElapsedMilliseconds + "ms save list");
			sw.Restart();

			// generate bytes
			bytes = new List<byte>(256);

			fn = @"delta\pal.dat";
			if (File.Exists(fn)) File.Delete(fn);

			// some code here to make PAL work
			byte bits = 0;
			double offset = 0d, defaultdelta = .226d;
			int ignore = 1, bitn = 7, skipped = 0;

			for(int i = 0;i < realframes;i += 8) {
				for(int y = 0;y < 8;y ++) {
					int value = 0, minoff = i < 800 ? -3 : 3;

					if (i + y + 1 < realframes) {
						int framed = totals[i + y];

						// this dumb. as it turns out, we need to handle some special cases
						/*	if(framed < MAXDELTAS[0]) {
								offset += defaultdelta;

							} else if(framed < MAXDELTAS[1] && framed > PALDELTAS[0]) {
								offset += defaultdelta;

							} else if (framed < MAXDELTAS[2] && framed > PALDELTAS[1]) {
								offset += defaultdelta;
							}*/
						offset += defaultdelta;

						// check the current and next frame
						if (offset > minoff && ignore <= 0 && framed + totals[i + y + 1] < PALDELTAS[0]) {
							// can duplicate
							offset -= 1d;
							value = 1;
							ignore = 1;
							skipped++;

						} else --ignore;
					}

					bits |= (byte) (value << bitn);
					
					// split bytes if needed
					if(--bitn < 0) {
						bytes.Add(bits);
						bits = 0;
						bitn = 7;
					}
				}
			}

			using (var f = File.OpenWrite(fn)) {
				f.Write(bytes.ToArray(), 0, bytes.Count);
				f.Flush();
				f.Close();
			}

			sw.Stop();
			Console.WriteLine(sw.ElapsedMilliseconds + "ms save pal "+ skipped +" / "+ realframes +" ("+ (Math.Round(1000d * (1d * realframes / (realframes - skipped) - 1)) / 10d) +"%)");
		}

		static void saveBig() {
			Stopwatch sw = new Stopwatch();
			sw.Restart();
			string fn1 = @"delta\data.dat";
			if (File.Exists(fn1)) File.Delete(fn1);

			string fn2 = @"delta\pointers.dat";
			if (File.Exists(fn2)) File.Delete(fn2);

			List<byte> f2dat = new List<byte>(1024);

			using (var f1 = File.OpenWrite(fn1)) {
				uint bank = 0x80000, addr = bank * 7, bx = 2;

				for (int i = 0; i < realframes; i++) {
					int len1 = saves[0][i].Length, len2 = saves[1][i].Length, len3 = saves[2][i].Length;

					if((addr & 1) != 0) {
						addr++;
						f1.WriteByte(0xFF);
					}
				
					if(addr + len1 + len2 + len3 >= bank * 8) {
						f1.Write(new byte[(bank * 8) - addr], 0, (int)((bank * 8) - addr));
						addr = bank * 7;
						bx++;
					}

					// save pointers to f2dat
					uint a = (uint) (addr + len3);
					f2dat.AddRange(new byte[] { (byte) bx, (byte) (a >> 16), (byte) (a >> 8), (byte) a });

					a = (uint) (addr);
					f2dat.AddRange(new byte[] { (byte) bx, (byte) (a >> 16), (byte) (a >> 8), (byte) a });

					a = (uint) (addr + len3 + len1);
					f2dat.AddRange(new byte[] { (byte) bx, (byte) (a >> 16), (byte) (a >> 8), (byte) a });

					// output data to f1
					f1.Write(saves[2][i], 0, saves[2][i].Length);
					f1.Write(saves[0][i], 0, saves[0][i].Length);
					f1.Write(saves[1][i], 0, saves[1][i].Length);
					addr += (uint) len1 + (uint) len2 + (uint) len3;
				}

				f1.Flush();
				f1.Close();
			}

			using (var f2 = File.OpenWrite(fn2)) {
				f2.Write(f2dat.ToArray(), 0, f2dat.Count);
				f2.Flush();
				f2.Close();
			}

			sw.Stop();
			Console.WriteLine(sw.ElapsedMilliseconds + "ms save datas");
		}
	}
}