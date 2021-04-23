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

namespace ConverterOld {
	class Tile {
		// d0 = black/white, d1 = grays
		public ulong d0, d1, xd0, xd1, yd0, yd1, xyd0, xyd1;

		public Tile() {
		}

		public void set(ulong _0, ulong _1) {
			d0 = _0;
			d1 = _1;
			x(d0, d1, out xd0, out xd1);
			y(d0, d1, out yd0, out yd1);
			y(xd0, xd1, out xyd0, out xyd1);
		}

		public void x(ulong d0, ulong d1, out ulong a0, out ulong a1) {
			a0 = _x(d0);
			a1 = _x(d1);
		}

		public ulong _x(ulong d) {
			return
				(d & 0x3) << 14 |				(d & 0xC) << 10 |				(d & 0x30) << 6 |				(d & 0xC0) << 2 |				(d & 0x300) >> 2 |				(d & 0xC00) >> 6 |				(d & 0x3000) >> 10 |			(d & 0xC000) >> 14 |
				(d & 0x30000) << 14 |			(d & 0xC0000) << 10 |			(d & 0x300000) << 6 |			(d & 0xC00000) << 2 |			(d & 0x3000000) >> 2 |			(d & 0xC000000) >> 6 |			(d & 0x30000000) >> 10 |		(d & 0xC0000000) >> 14 |
				(d & 0x300000000) << 14 |		(d & 0xC00000000) << 10 |		(d & 0x3000000000) << 6 |		(d & 0xC000000000) << 2 |		(d & 0x30000000000) >> 2 |		(d & 0xC0000000000) >> 6 |		(d & 0x300000000000) >> 10 |	(d & 0xC00000000000) >> 14 |
				(d & 0x3000000000000) << 14 |	(d & 0xC000000000000) << 10 |	(d & 0x30000000000000) << 6 |	(d & 0xC0000000000000) << 2 |	(d & 0x300000000000000) >> 2 |	(d & 0xC00000000000000) >> 6 |	(d & 0x3000000000000000) >> 10 |(d & 0xC000000000000000) >> 14;
		}

		public void y(ulong d0, ulong d1, out ulong a0, out ulong a1) {
			a0 = _y(d0);
			a1 = _y(d1);
		}

		public ulong _y(ulong d) {
			return ((d & 0xFFFF) << 48) | ((d & 0xFFFF0000) << 16) | ((d & 0xFFFF00000000) >> 16) | ((d & 0xFFFF000000000000) >> 48);
		}
	}

	class Video {
		static int THREADS = Environment.ProcessorCount, FOPT = THREADS;
		const int SIMILAR = 10;
		const int OPT = 3;
		const int DIV = 8;
		static string[] files;
		static List<Tile>[] planes;

		static void _Main(string[] args) {
			Stopwatch sw = new Stopwatch();
			sw.Start();
			Console.WriteLine("Start video encode with " + THREADS + " threads!");

			files = Directory.GetFiles(@"A:\badapple\img");
			planes = new List<Tile>[files.Length];

			Thread[] threads = new Thread[THREADS];
			src = new List<Tile>[THREADS];

			for (int i = 0; i < THREADS; i++) {
				threads[i] = new Thread((x) => { 
					executeFiles(int.Parse((string)x));
				}) {
					Priority = ThreadPriority.BelowNormal
				};
			}

			for (int i = 0; i < THREADS; i++)
				threads[i].Start(i +"");

			for (int i = 0; i < THREADS; i++)
				threads[i].Join();

			Console.WriteLine("");

			for (int i = 0; i < FOPT; i++) {
				Console.WriteLine("round " + (i + 1) + " / " + (OPT + FOPT));
				OptimizeAll2(THREADS);
			}

			for (int i = 0;i < OPT; i++) {
				Console.WriteLine("round " + (i + FOPT + 1) + " / " + (OPT + FOPT));
				OptimizeAll(THREADS);
			}

			Console.WriteLine("final combine");
			FinalOptimize();
			sw.Stop();
			Console.WriteLine("found "+ dst[0].Count +" tiles in "+ (sw.ElapsedMilliseconds / 1000f) +"s");
			dst[1][0] = new Tile();
			Console.ReadKey();
		}

		static void executeFiles(int ti) {
			Stopwatch sw = new Stopwatch();
			src[ti] = new List<Tile>(100);
			int i = ti;

			for (; i < files.Length / DIV; i += THREADS) {
				sw.Restart();
				Bitmap b = (Bitmap) Image.FromFile(files[i]);
				BitmapData bd = b.LockBits(new Rectangle(0, 0, b.Width, b.Height), ImageLockMode.ReadWrite, b.PixelFormat);

				List<Tile> tiles = new List<Tile>(1200);
				for (int x = 0; x < 1200; x++)
					tiles.Add(new Tile());

				unsafe {
					byte* px = (byte*) bd.Scan0;

					for (int y = 0; y < 240;y += 8) {
						for (int x = 0; x < 320;x += 8) {
							ulong d0 = 0, d1 = 0;

							for(int _y = 0;_y < 8; _y++) {
								for (int _x = 0; _x < 8; _x++) {

									int c = getColor(px[((y + _y) * bd.Stride) + ((x + _x) * 4) + 1]);
									d0 |= d0tab[c] << (_x + (_y * 8));
									d1 |= d1tab[c] << (_x + (_y * 8));
								}
							}

							tiles[(x / 8) + (y / 8 * 40)].set(d0, d1);
						}
					}
				}

				b.UnlockBits(bd);
				b.Dispose();

				// optimize tiles
				int total = 1200, unq = 0;

				for (int z = 0; z < 1200; z++) {
					bool check = true;

					for (int x = 0; x < z; x++) {
						// check if these are the same
						if (cmp(tiles[z], tiles[x])) {
							tiles[z] = tiles[x];
							--total;
							check = false;
							break;
						}
					}

					if (check) {
						bool add = true;

						for (int x = 0; x < src[ti].Count; x++) {
							if (cmp(tiles[z], src[ti][x])) {
								add = false;
							}
						}

						if (add) {
							src[ti].Add(tiles[z]);
							unq++;
						}
					}
				}

				planes[i] = tiles;
				sw.Stop();
				Console.WriteLine(sw.ElapsedMilliseconds +"ms "+ total + " ("+ unq +" unique) tiles in " + files[i]);
				

/*				if(i == 1404) {
					Console.WriteLine("WRITE IMAGE "+ total);
					Bitmap q = new Bitmap(320, 240, PixelFormat.Format32bppArgb);

					for (int u = 0; u < tiles.Count; u++) {
						for (int x = 0; x < 64; x++) {
							byte z = (byte) ((tiles[u].data[x / 2] >> ((x & 1) * 4)) & 0x3);
							int argb = (z * (255 / 3));

							q.SetPixel((u % 40 * 8) + (x & 7), (u / 40 * 8) + (x / 8), Color.FromArgb(0xFF, argb, argb, argb));
						}
					}

					q.Save(@"G:\RESEARCH\bad apple\test.png", ImageFormat.Png);
					q.Dispose();
				}*/
			}
		}

		static ulong[] d0tab = { 0, 0, 1, 1 };
		static ulong[] d1tab = { 0, 1, 1, 0 };

		static int getColor(byte p) {
			return (p / 85);
		}

		static bool cmp(Tile a, Tile b) {
			return a.d1 == b.d1 && a.d0 == b.d0;
		}

		static bool cmpall(Tile a, Tile b) {
			if (a == null || b == null) return false;
			if (getDelta(a.d0, a.d1, b.d0, b.d1) <= SIMILAR) return true;
			if (getDelta(a.xd0, a.xd1, b.xd0, b.xd1) <= SIMILAR) return true;
			if (getDelta(a.yd0, a.yd1, b.yd0, b.yd1) <= SIMILAR) return true;
			if (getDelta(a.xyd0, a.xyd1, b.xyd0, b.xyd1) <= SIMILAR) return true;
			return false;
		}

		static int getDelta(ulong a0, ulong a1, ulong b0, ulong b1) {
			return getBits(a0, b0) * 3 + getBits(a1, b1);
		}

		static int getBits(ulong a, ulong b) {
			int r = 0;

			for (ulong i = 0x8000000000000000; i > 0; i >>= 1)
				if ((a & i) != (b & i)) 
					r++;

			return r;
		}

		static List<Tile>[] src, dst;

		static void OptimizeAll2(int dlen) {
			dst = new List<Tile>[dlen];
			Thread[] threads = new Thread[dlen];

			for (int i = 0; i < dlen; i++) {
				threads[i] = new Thread((x) => {
					Optimize2(int.Parse((string) x), dlen);
				}) {
					Priority = ThreadPriority.BelowNormal
				};
			}

			for (int i = 0; i < dlen; i++)
				threads[i].Start(i + "");

			for (int i = 0; i < dlen; i++)
				threads[i].Join();

			Console.WriteLine("");
			src = dst;
		}

		static void Optimize2(int ti, int offs) {
			Stopwatch sw = new Stopwatch();
			sw.Start();
			dst[ti] = new List<Tile>(5000);
			int s = 0;

			for (int p = 0; p < src.Length; p++) {
				for (int i = ti; i < src[p].Count; i += offs) {
					s++;
					bool add = true;

					for (int x = 0; x < dst[ti].Count; x++) {
						// check if these are the same
						if (cmp(src[p][i], dst[ti][x])) {
							add = false;
							break;
						}
					}

					if (add) {
						dst[ti].Add(src[p][i]);
					}
				}
			}

			sw.Stop();
			Console.WriteLine("thread " + ti.ToString("X2") + ": " + s + " -> " + dst[ti].Count + " in " + (sw.ElapsedMilliseconds / 1000f) + "s");
		}

		static void OptimizeAll(int dlen) {
			dst = new List<Tile>[dlen];
			Thread[] threads = new Thread[dlen];

			for (int i = 0; i < dlen; i++) {
				threads[i] = new Thread((x) => {
					Optimize(int.Parse((string) x), dlen);
				}) {
					Priority = ThreadPriority.BelowNormal
				};
			}

			for (int i = 0; i < dlen; i++)
				threads[i].Start(i + "");

			for (int i = 0; i < dlen; i++)
				threads[i].Join();

			Console.WriteLine("");
			src = dst;
		}

		static void Optimize(int ti, int offs) {
			Stopwatch sw = new Stopwatch();
			sw.Start();
			dst[ti] = new List<Tile>(5000);
			int s = 0;

			for (int p = 0; p < src.Length; p++) {
				for (int i = ti; i < src[p].Count; i += offs) {
					s ++;
					bool add = true;

					for (int x = 0; x < dst[ti].Count; x++) {
						// check if these are the same
						if (cmpall(src[p][i], dst[ti][x])) {
							add = false;
							break;
						}
					}

					if (add) {
						dst[ti].Add(src[p][i]);
					}
				}
			}

			sw.Stop();
			Console.WriteLine("thread " + ti.ToString("X2") + ": " + s + " -> " + dst[ti].Count +" in "+ (sw.ElapsedMilliseconds / 1000f) + "s");
		}

		static void FinalOptimize() {
			Stopwatch sw = new Stopwatch();
			sw.Start();

			dst = new List<Tile>[] {
				new List<Tile>(5000)
			};

			Thread[] threads = new Thread[THREADS];

			for (int i = 0; i < THREADS; i++) {
				threads[i] = new Thread((x) => {
					FinalOpt(int.Parse((string) x), THREADS);
				}) {
					Priority = ThreadPriority.BelowNormal
				};
			}

			for (int i = 0; i < THREADS; i++)
				threads[i].Start(i + "");

			for (int i = 0; i < THREADS; i++)
				threads[i].Join();

			src = dst;
			sw.Stop();
			Console.WriteLine("combine to "+ dst[0].Count +" in " + (sw.ElapsedMilliseconds / 1000f) + "s");
		}

		static object locker = new object();

		static void FinalOpt(int ti, int offs) {
			int s = 0;

			for (int p = 0; p < src.Length; p++) {
				for (int i = ti; i < src[p].Count; i += offs) {
					s++;
					bool add = true;

					for (int x = 0; x < dst[0].Count; x++) {
						// check if these are the same
						if (cmpall(src[p][i], dst[0][x])) {
							add = false;
							break;
						}
					}

					if (add) {
						lock (locker) {
							dst[0].Add(src[p][i]);
						}
					}
				}
			}

		}

		/*	static void FinalOptimize() {
				Stopwatch sw = new Stopwatch();
				sw.Start();
				dst[0] = new List<Tile>(5000);
				int s = 0;

				for (int p = 0; p < src.Length; p++) {
					for (int i = 0; i < src[p].Count; i ++) {
						s++;
						dst[0].Add(src[p][i]);
					}
				}

				sw.Stop();
				Console.WriteLine("final: " + s + " -> " + dst[0].Count + " in " + (sw.ElapsedMilliseconds / 1000f) + "s");
			}*/
	}
}