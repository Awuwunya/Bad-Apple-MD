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

namespace ConverterOld2{
	class Tile {
		static byte[] rev = {
			0x00, 0x80, 0x40, 0xC0, 0x20, 0xA0, 0x60, 0xE0, 0x10, 0x90, 0x50, 0xD0, 0x30, 0xB0, 0x70, 0xF0,
			0x08, 0x88, 0x48, 0xC8, 0x28, 0xA8, 0x68, 0xE8, 0x18, 0x98, 0x58, 0xD8, 0x38, 0xB8, 0x78, 0xF8,
			0x04, 0x84, 0x44, 0xC4, 0x24, 0xA4, 0x64, 0xE4, 0x14, 0x94, 0x54, 0xD4, 0x34, 0xB4, 0x74, 0xF4,
			0x0C, 0x8C, 0x4C, 0xCC, 0x2C, 0xAC, 0x6C, 0xEC, 0x1C, 0x9C, 0x5C, 0xDC, 0x3C, 0xBC, 0x7C, 0xFC,
			0x02, 0x82, 0x42, 0xC2, 0x22, 0xA2, 0x62, 0xE2, 0x12, 0x92, 0x52, 0xD2, 0x32, 0xB2, 0x72, 0xF2,
			0x0A, 0x8A, 0x4A, 0xCA, 0x2A, 0xAA, 0x6A, 0xEA, 0x1A, 0x9A, 0x5A, 0xDA, 0x3A, 0xBA, 0x7A, 0xFA,
			0x06, 0x86, 0x46, 0xC6, 0x26, 0xA6, 0x66, 0xE6, 0x16, 0x96, 0x56, 0xD6, 0x36, 0xB6, 0x76, 0xF6,
			0x0E, 0x8E, 0x4E, 0xCE, 0x2E, 0xAE, 0x6E, 0xEE, 0x1E, 0x9E, 0x5E, 0xDE, 0x3E, 0xBE, 0x7E, 0xFE,
			0x01, 0x81, 0x41, 0xC1, 0x21, 0xA1, 0x61, 0xE1, 0x11, 0x91, 0x51, 0xD1, 0x31, 0xB1, 0x71, 0xF1,
			0x09, 0x89, 0x49, 0xC9, 0x29, 0xA9, 0x69, 0xE9, 0x19, 0x99, 0x59, 0xD9, 0x39, 0xB9, 0x79, 0xF9,
			0x05, 0x85, 0x45, 0xC5, 0x25, 0xA5, 0x65, 0xE5, 0x15, 0x95, 0x55, 0xD5, 0x35, 0xB5, 0x75, 0xF5,
			0x0D, 0x8D, 0x4D, 0xCD, 0x2D, 0xAD, 0x6D, 0xED, 0x1D, 0x9D, 0x5D, 0xDD, 0x3D, 0xBD, 0x7D, 0xFD,
			0x03, 0x83, 0x43, 0xC3, 0x23, 0xA3, 0x63, 0xE3, 0x13, 0x93, 0x53, 0xD3, 0x33, 0xB3, 0x73, 0xF3,
			0x0B, 0x8B, 0x4B, 0xCB, 0x2B, 0xAB, 0x6B, 0xEB, 0x1B, 0x9B, 0x5B, 0xDB, 0x3B, 0xBB, 0x7B, 0xFB,
			0x07, 0x87, 0x47, 0xC7, 0x27, 0xA7, 0x67, 0xE7, 0x17, 0x97, 0x57, 0xD7, 0x37, 0xB7, 0x77, 0xF7,
			0x0F, 0x8F, 0x4F, 0xCF, 0x2F, 0xAF, 0x6F, 0xEF, 0x1F, 0x9F, 0x5F, 0xDF, 0x3F, 0xBF, 0x7F, 0xFF
		};

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
				((ulong) rev[(d & 0xFF)]) |
				((ulong) rev[(d & 0xFF00) >> 8] << 8) |
				((ulong) rev[(d & 0xFF0000) >> 16] << 16) |
				((ulong) rev[(d & 0xFF000000) >> 24] << 24) |
				((ulong) rev[(d & 0xFF00000000) >> 32] << 32) |
				((ulong) rev[(d & 0xFF0000000000) >> 40] << 40) |
				((ulong) rev[(d & 0xFF000000000000) >> 48] << 48) |
				((ulong) rev[(d & 0xFF00000000000000) >> 56] << 56);
		}

		public void y(ulong d0, ulong d1, out ulong a0, out ulong a1) {
			a0 = _y(d0);
			a1 = _y(d1);
		}

		public ulong _y(ulong d) {
			return 
				((d & 0xFF) << 56) |
				((d & 0xFF00) << 40) |
				((d & 0xFF0000) << 24) |
				((d & 0xFF000000) << 8) |
				((d & 0xFF00000000) >> 8) |
				((d & 0xFF0000000000) >> 24) |
				((d & 0xFF000000000000) >> 40) |
				((d & 0xFF00000000000000) >> 56);
		}
	}

	class TileDelta {
		public ushort vram;
		public byte d0, d1;

		public TileDelta(ushort vram, byte d0, byte d1) {
			this.vram = vram;
			this.d0 = d0;
			this.d1 = d1;
		}

		public byte[] getResult() {
			byte[] a = new byte[] { 0, 0 };

			// collect bytes
			a[0] |= (byte) ((d0 & 0x80) | ((d1 & 0x80) >> 1));			// xx000000
			a[0] |= (byte) (((d0 & 0x40) >> 1) | ((d1 & 0x40) >> 2));	// xxyy0000
			a[0] |= (byte) (((d0 & 0x20) >> 2) | ((d1 & 0x20) >> 3));	// xxyyzz00
			a[0] |= (byte) (((d0 & 0x10) >> 3) | ((d1 & 0x10) >> 4));	// xxyyzzqq

			a[1] |= (byte) (((d0 & 0x08) << 4) | ((d1 & 0x08) << 3));	// xx000000
			a[1] |= (byte) (((d0 & 0x04) << 3) | ((d1 & 0x04) << 2));	// xxyy0000
			a[1] |= (byte) (((d0 & 0x02) << 2) | ((d1 & 0x02) << 1));	// xxyyzz00
			a[1] |= (byte) (((d0 & 0x01) << 1) | (d1 & 0x01));			// xxyyzz00
			return a;
		}
	}

	class PlaneDelta {
		public ushort offset;
		public ushort value;

		public PlaneDelta(ushort offset, ushort value) {
			this.offset = offset;
			this.value = value;
		}
	}

	class Video {
		static int THREADS = Environment.ProcessorCount;
		const int SIMILAR = 11, PLANES = 2, TPC = 0x300;
		const int DIV = 50;
		static string[] files;

		static void Main(string[] args) {
			Stopwatch sw = new Stopwatch();
			sw.Start();
			Console.WriteLine("Start video encode with " + THREADS + " threads!");

			files = Directory.GetFiles(@"A:\badapple\img");
			plane = new ushort[files.Length][];
			tiles = new List<Tile>[files.Length];

			Thread[] threads = new Thread[THREADS * 4];

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

			screen = new List<Tile>[PLANES];
			tpre = new List<Tile>[files.Length];
			td = new List<TileDelta>[files.Length];
			pd = new List<PlaneDelta>[files.Length];

			for (int i = 0; i < PLANES; i++) {
				threads[i] = new Thread((x) => {
					runPlanes(int.Parse((string) x), PLANES);
				}) {
					Priority = ThreadPriority.AboveNormal
				};
			}

			for (int i = 0; i < PLANES; i++)
				threads[i].Start(i + "");

			for (int i = 0; i < PLANES; i++)
				threads[i].Join();

			for (int i = 0; i < THREADS * 4; i++) {
				threads[i] = new Thread((x) => {
					save(int.Parse((string) x), THREADS * 4);
				}) {
					Priority = ThreadPriority.BelowNormal
				};
			}

			for (int i = 0; i < THREADS * 4; i++)
				threads[i].Start(i + "");

			for (int i = 0; i < THREADS * 4; i++)
				threads[i].Join();

			sw.Stop();
			Console.WriteLine("done in "+ (sw.ElapsedMilliseconds / 1000f) +"s");
			Console.ReadKey();
		}

		static void save(int ti, int off) {
			Stopwatch sw = new Stopwatch();
			int i = ti;

			for (; i < files.Length / DIV; i += off) {
				sw.Restart();

				string fn = @"G:\RESEARCH\bad apple\delta\t" + i.ToString("X4") + ".delta.dat";
				if (File.Exists(fn)) File.Delete(fn);

				// generate lists based on size
				List<byte[]>[] _lists = new List<byte[]>[] { 
					new List<byte[]>(512),	// 1
					new List<byte[]>(256),	// 2
					new List<byte[]>(64),	// 4
					new List<byte[]>(32),	// 8
				};

				List<TileDelta> prg = new List<TileDelta>();

				foreach (TileDelta x in td[i].OrderBy((TileDelta t) => t.vram)) {
					// grab a new delta program
					if(prg.Count == 0) {
						prg.Add(x);
						continue;
					}

					if(x.vram == prg[0].vram + (prg.Count * 4)) {
						// this follows
						if(prg.Count >= 8) {
							// must be split
							_lists[3].Add(getPrgBytes(prg));
							prg = new List<TileDelta>();
						}

						// does not need to be split
						prg.Add(x);

					} else if ((prg.Count == 2 && x.vram == prg[0].vram + 16) || prg.Count == 6 && x.vram == prg[0].vram + 32) {
						// insert data from the screen
						int _vram = x.vram - 4;
						Tile t = tpre[i][_vram / 32];
						prg.Add(new TileDelta((ushort)_vram, (byte)((t.d0 >> ((_vram & 0x1C) >> 2)) & 0xFF), (byte) ((t.d1 >> ((_vram & 0x1C) >> 2)) & 0xFF)));
						prg.Add(x);

					} else {
						// split data
						int list = 0;
						switch(prg.Count) {
							case 1: list = 0; break;
							case 2: list = 1; break;
							case 4: list = 2; break;
							case 8: list = 3; break;

							case 5: {
								List<TileDelta> l = new List<TileDelta>() {
									prg[0], prg[1], prg[2], prg[3]
								};

								_lists[2].Add(getPrgBytes(l));
								prg = new List<TileDelta>() { prg[4] };
								list = 0;
								break;
							}

							case 6: {
								List<TileDelta> l = new List<TileDelta>() {
									prg[0], prg[1], prg[2], prg[3]
								};

								_lists[2].Add(getPrgBytes(l));
								prg = new List<TileDelta>() { prg[4], prg[5] };
								list = 1;
								break;
							}

							case 3: {
								int _vram = prg[prg.Count - 1].vram + 4;

								if (_vram / 32 >= TPC) {
									// fuck
									_lists[0].Add(getPrgBytes(new List<TileDelta>() { prg[6] }));
									prg = new List<TileDelta>() { prg[4], prg[5] };
									list = 1;

								} else {
									Tile t = tpre[i][_vram / 32];
									prg.Add(new TileDelta((ushort) _vram, (byte) ((t.d0 >> ((_vram & 0x1C) >> 2)) & 0xFF), (byte) ((t.d1 >> ((_vram & 0x1C) >> 2)) & 0xFF)));
									list = 2;
								}
								break;
							}

							case 7: {
								int _vram = prg[prg.Count - 1].vram + 4;

								if (_vram / 32 >= TPC) {
									// fuck
									_lists[1].Add(getPrgBytes(new List<TileDelta>() { prg[4], prg[5] }));
									_lists[0].Add(getPrgBytes(new List<TileDelta>() { prg[6] }));
									prg = new List<TileDelta>() { prg[0], prg[1], prg[2], prg[3] };
									list = 2;

								} else {
									Tile t = tpre[i][_vram / 32];
									prg.Add(new TileDelta((ushort) _vram, (byte) ((t.d0 >> ((_vram & 0x1C) >> 2)) & 0xFF), (byte) ((t.d1 >> ((_vram & 0x1C) >> 2)) & 0xFF)));
									list = 3;
								}

								break;
							}

							default:
								Console.WriteLine("------- split " + prg.Count);
							//	Console.ReadKey();
								break;
						}

						_lists[list].Add(getPrgBytes(prg));
						prg = new List<TileDelta>() { x };
					}
				}

				switch (prg.Count) {
					case 1:
						_lists[0].Add(getPrgBytes(prg));
						break;

					case 2:
						_lists[1].Add(getPrgBytes(prg));
						break;

					case 4:
						_lists[2].Add(getPrgBytes(prg));
						break;

					case 8:
						_lists[3].Add(getPrgBytes(prg));
						break;

					case 5: {
						List<TileDelta> l = new List<TileDelta>() {
							prg[0], prg[1], prg[2], prg[3]
						};

						_lists[2].Add(getPrgBytes(l));
						_lists[0].Add(getPrgBytes(new List<TileDelta>() { prg[4] }));
						break;
					}

					case 6: {
						List<TileDelta> l = new List<TileDelta>() {
							prg[0], prg[1], prg[2], prg[3]
						};

						_lists[2].Add(getPrgBytes(l));
						_lists[0].Add(getPrgBytes(new List<TileDelta>() { prg[4], prg[5] }));
						break;
					}

					case 3: {
						int _vram = prg[prg.Count - 1].vram + 4;

						if (_vram / 32 >= TPC) {
							// fuck
							_lists[1].Add(getPrgBytes(new List<TileDelta>() { prg[0], prg[1] }));
							_lists[0].Add(getPrgBytes(new List<TileDelta>() { prg[2] }));

						} else {
							Tile t = tpre[i][_vram / 32];
							prg.Add(new TileDelta((ushort) _vram, (byte) ((t.d0 >> ((_vram & 0x1C) >> 2)) & 0xFF), (byte) ((t.d1 >> ((_vram & 0x1C) >> 2)) & 0xFF)));
							_lists[2].Add(getPrgBytes(prg));
						}
						break;
					}

					case 7: {
						int _vram = prg[prg.Count - 1].vram + 4;

						if (_vram / 32 >= TPC) {
							// fuck
							_lists[2].Add(getPrgBytes(new List<TileDelta>() { prg[0], prg[1], prg[2], prg[3] }));
							_lists[1].Add(getPrgBytes(new List<TileDelta>() { prg[4], prg[5] }));
							_lists[0].Add(getPrgBytes(new List<TileDelta>() { prg[6] }));

						} else { 
							Tile t = tpre[i][_vram / 32];
							prg.Add(new TileDelta((ushort) _vram, (byte) ((t.d0 >> ((_vram & 0x1C) >> 2)) & 0xFF), (byte) ((t.d1 >> ((_vram & 0x1C) >> 2)) & 0xFF)));
							_lists[3].Add(getPrgBytes(prg));
						}
						break;
					}

					default:
						Console.WriteLine("------- last " + prg.Count);
					//	Console.ReadKey();
						break;

					// ignore
					case 0: break;
				}

				// generate bytes
				List<byte> bytes = new List<byte>(1024);

				for(int z = _lists.Length -1; z >= 0 ;z--) {
					bytes.Add((byte) ((_lists[z].Count - 1) >> 8));
					bytes.Add((byte) ((_lists[z].Count - 1) & 0xFF));

					foreach (byte[] b in _lists[z])
						bytes.AddRange(b);
				}

				// flush bytes
				using (var f = File.OpenWrite(fn)) {
					f.Write(bytes.ToArray(), 0, bytes.Count);
					f.Flush();
					f.Close();					
				}

				sw.Stop();
				Console.WriteLine(sw.ElapsedMilliseconds + "ms " + bytes.Count + " bytes in " + fn);
			}
		}

		static byte[] getPrgBytes(List<TileDelta> p) {
			byte[] ret = new byte[2 + (p.Count * 2)];
			ret[0] = (byte) (p[0].vram >> 8);
			ret[1] = (byte) (p[0].vram & 0xFF);

			for (int i = 0;i < p.Count;i++) {
				var r = p[i].getResult();
				ret[i * 2 + 2] = r[0];
				ret[i * 2 + 3] = r[1];
			}

			return ret;
		}

		static List<Tile>[] tiles;
		static List<Tile>[] screen;
		static ushort[][] plane;
		static List<TileDelta>[] td;
		static List<Tile>[] tpre;
		static List<PlaneDelta>[] pd;

		static void runPlanes(int ti, int off) {
			Stopwatch sw = new Stopwatch();
			int i = ti, maxttl = 0;

			screen[ti] = new List<Tile>(TPC);
			long[] ms = new long[files.Length / off + off];

			for (int x = 0; x < TPC; x++)
				screen[ti].Add(new Tile());

			for (; i < files.Length / DIV; i += off) {
				sw.Restart();
				bool[] skip = new bool[TPC];

				tpre[i] = new List<Tile>(TPC);

				// copy screen here
				for(int a = 0;a < TPC;a++)
					tpre[i].Add(screen[ti][a]);

				td[i] = new List<TileDelta>(256);
				pd[i] = new List<PlaneDelta>(128);

				for (int x = 0;x < tiles[i].Count; x++) {
					bool add = true;
					int mindiff = 0xFFFF, pos = 0, _bits = 0, _rows = 0;

					for (int y = 0; y < TPC; y++) {
						if (skip[y]) continue;

						int diff;
						if ((diff = getMods(tiles[i][x], screen[ti][y], out int bits, out int rows)) == 0) {
							add = false;
							skip[y] = true;
							break;
						}

						if (diff < mindiff) {
							_bits = bits;
							_rows = rows;
							mindiff = diff;
							pos = y;
						}
					}

					if(add) {
						addTileDelta(ti, pos, i, x, _bits, _rows);

						skip[pos] = true;
						screen[ti][pos] = tiles[i][x];
					}
				}

				sw.Stop();
				if (maxttl < td[i].Count) maxttl = td[i].Count;
				ms[i / off] = td[i].Count;

				long avg = 0;

				for (int a = 0; a < i / off; a++)
					avg += ms[a];

				Console.WriteLine(sw.ElapsedMilliseconds + "ms " + td[i].Count + " (max: "+ maxttl +", avg: "+ (avg / ((i / off) + 1)) +") tiles in " + files[i]);
			}
		}

		static void addTileDelta(int ti, int pos, int i, int x, int bits, int rows) {
			// check all rows backward
			for (int z = 0, r = 0x80;r > 0;r >>= 1, z++) {
				if((rows & r) != 0) {
					// row active, mod tile
					td[i].Add(new TileDelta((ushort)((pos * 32) + ((bits & 0x1800) == 0 ? z : 7 - z) * 4), (byte)((tiles[i][x].d0 >> r) & 0xFF), (byte)((tiles[i][x].d1 >> r) & 0xFF)));
				}
			}
		}

		// TODO: calculate delta here
		static int getMods(Tile a, Tile b, out int bits, out int rows) {
			int diff1, diff2, diff3, diff4;
			rows = 0;

			if ((diff1 = getMod(a.d0, a.d1, b.d0, b.d1, out int rows1)) == 0) {
				bits = 0;
				return 0;
			}

			if ((diff2 = getMod(a.xd0, a.xd1, b.xd0, b.xd1, out int rows2)) == 0) {
				bits = 0x800;
				return 0;
			}

			if ((diff3 = getMod(a.yd0, a.yd1, b.yd0, b.yd1, out int rows3)) == 0) {
				bits = 0x1000;
				return 0;
			}

			if ((diff4 = getMod(a.xyd0, a.xyd1, b.xyd0, b.xyd1, out int rows4)) == 0) {
				bits = 0x1800;
				return 0;
			}

			// horrible code here
			if (diff1 < diff2) {
				if (diff1 < diff3) {
					if (diff1 < diff4) {
						bits = 0;
						rows = rows1;
						return diff1;

					} else goto _rows4;

				} else if (diff3 < diff4) {
					goto _rows3;

				} else goto _rows4;

			} else if (diff2 < diff3) {
				if (diff2 < diff4) {
					bits = 0x800;
					rows = rows2;
					return diff2;

				} else goto _rows4;

			} else goto _rows3;

			_rows3:
			bits = 0x1000;
			rows = rows3;
			return diff3;

			_rows4:
			bits = 0x1800;
			rows = rows4;
			return diff4;
		}

		static int getMod(ulong a0, ulong a1, ulong b0, ulong b1, out int rows) {
			int r = 0, x;
			rows = 0;

			if((x = getModRow(0, a0, b0, a1, b1)) != 0) {
				rows |= 1;
				r += x;
			}

			if((x = getModRow(8, a0, b0, a1, b1)) != 0) {
				rows |= 2;
				r += x;
			}

			if((x = getModRow(16, a0, b0, a1, b1)) != 0) {
				rows |= 4;
				r += x;
			}

			if ((x = getModRow(24, a0, b0, a1, b1)) != 0) {
				rows |= 8;
				r += x;
			}

			if ((x = getModRow(32, a0, b0, a1, b1)) != 0) {
				rows |= 0x10;
				r += x;
			}

			if ((x = getModRow(40, a0, b0, a1, b1)) != 0) {
				rows |= 0x20;
				r += x;
			}

			if ((x = getModRow(48, a0, b0, a1, b1)) != 0) {
				rows |= 0x40;
				r += x;
			}

			if ((x = getModRow(56, a0, b0, a1, b1)) != 0) {
				rows |= 0x80;
				r += x;
			}
			return r;
		}

		static int getModRow(int shift, ulong a0, ulong a1, ulong b0, ulong b1) {
			return getModBits((byte) ((a0 >> shift) & 0xFF), (byte) ((b0 >> shift) & 0xFF)) * 3 + getBits((byte) ((a1 >> shift) & 0xFF), (byte) ((b1 >> shift) & 0xFF));
		}

		static int getModBits(byte a, byte b) {
			int r = 0;

			for (byte i = 0x80; i > 0; i >>= 1)
				if ((a & i) != (b & i))
					r++;

			return r;
		}

		static void executeFiles(int ti) {
			Stopwatch sw = new Stopwatch();
			int i = ti;

			for (; i < files.Length / DIV; i += THREADS) {
				sw.Restart();
				Bitmap b = (Bitmap) Image.FromFile(files[i]);
				BitmapData bd = b.LockBits(new Rectangle(0, 0, b.Width, b.Height), ImageLockMode.ReadWrite, b.PixelFormat);

				tiles[i] = new List<Tile>(100);
				plane[ti] = new ushort[1200];

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

							// find tile
							Tile t = new Tile();
							t.set(d0, d1);
							bool add = true;

							for (ushort v = 0; v < tiles[i].Count; v++) {
								// check if these are the same
								ushort o;
								if ((o = cmpall(tiles[i][v], t, out int diff)) != 0xFFFF) {
									plane[ti][(x / 8) + (y / 8 * 40)] = (ushort)(v | o);
									add = false;
									break;
								}
							}

							if(add) {
								plane[ti][(x / 8) + (y / 8 * 40)] = (ushort) tiles[i].Count;
								tiles[i].Add(t);
							}
						}
					}
				}

				b.UnlockBits(bd);
				b.Dispose();
				sw.Stop();
				Console.WriteLine(sw.ElapsedMilliseconds +"ms "+ tiles[i].Count + " tiles in " + files[i]);
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

		static ushort cmpall(Tile a, Tile b, out int diff) {
			if ((diff = getDelta(a.d0, a.d1, b.d0, b.d1)) <= SIMILAR) return 0;
			if ((diff = getDelta(a.xd0, a.xd1, b.xd0, b.xd1)) <= SIMILAR) return 0x800;
			if ((diff = getDelta(a.yd0, a.yd1, b.yd0, b.yd1)) <= SIMILAR) return 0x1000;
			if ((diff = getDelta(a.xyd0, a.xyd1, b.xyd0, b.xyd1)) <= SIMILAR) return 0x1800;
			return (ushort)(diff = 0xFFFF);
		}

		static int getDelta(ulong a0, ulong a1, ulong b0, ulong b1) {
			//	return getBits(a0, b0) * 3 + getBits(a1, b1);
			return (a0 == b0 && a1 == b1) ? 0 : 0xFFFF;
		}

		static int getBits(ulong a, ulong b) {
			int r = 0;

			for (ulong i = 0x8000000000000000; i > 0; i >>= 1)
				if ((a & i) != (b & i)) 
					r++;

			return r;
		}
	}
}