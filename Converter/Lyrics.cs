using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Converter {
	class Sprite {
		public int height, width = Lyrics.width / 8;
		public int y, x;
		public byte[] data;		// note: not in the correct format

		public Sprite(int height, int x, int y, byte[] data) {
			this.height = height;
			this.x = x;
			this.y = y;
			this.data = data;
		}

		public byte[] getData() {
			byte[] d = new byte[32 * width * height];

			for (int tx = 0, p = 0; tx < width; tx++)
				for (int ty = 0;ty < height;ty++) {
					for(int yy = 0;yy < 8; yy++) {
						for(int xx = 0;xx < 4;xx++) {
							d[p++] = data[xx + ((ty * 8 + yy) * width * 4) + (tx * 4)];
						}
					}
				}

			return d;
		}

		public byte[] getSprite(ref int vram, ref byte link, int left) {
			byte[] d = new byte[8];

			int _y = 0x80 + y;
			d[0] = (byte) (_y >> 8);
			d[1] = (byte) _y;

			d[2] = (byte) (((width - 1) << 2) | (height - 1));
			d[3] = ++link;

			d[4] = (byte) (vram >> 8);
			d[5] = (byte) vram;
			vram += width * height;

			int _x = 0x80 + x - (left == 0 ? 12 : 320 - 16 - left);
			d[6] = (byte) (_x >> 8);
			d[7] = (byte) _x;
			return d;
		}
	}

	class Lyrics {
		public const byte TRANSPARENCY = 0x80;
		public static int[] rows = { 311 - 1, 289 - 1, 267 - 1 };
		public const int width = 16, vram = 0x480;

		public static int getCol(byte c) {
			if (c == TRANSPARENCY) return 0;
			return table[c / 32];
		}

		static int[] table = { 1, 2, 3, 4, 5, 6, 7, 8, 9 };

		static void Main(string[] args) {
			Stopwatch sw = new Stopwatch();
			sw.Start();
			string[] files = Directory.GetFiles(@"lyrics\png");
			int maxtile = 0;

			foreach(string png in files) {
				Bitmap b = (Bitmap) Image.FromFile(png);
				BitmapData bd = b.LockBits(new Rectangle(0, 0, b.Width, b.Height), ImageLockMode.ReadWrite, b.PixelFormat);

				// need to investigate color positions
				List<Sprite> sprites = new List<Sprite>();
				int tilec = 0;

				unsafe {
					byte* px = (byte*) bd.Scan0;
					int s = Image.GetPixelFormatSize(bd.PixelFormat) / 8;
					int top = -1;

					for (int r = 0; r < rows.Length; r++) {
						int tilestart = 0;

						for (int y = 0; y < bd.Height; y++) {

							// skip checking other than tile boundaries
							if (tilestart > 0 && ((y - tilestart) % 8) != 0)
								continue;

							bool color = false;

							// check for color
							for (int x = rows[r] - (width - 1); !color && x <= rows[r]; x++) {
								for(int yo = 0; !color && yo < 8 && yo + y < bd.Height; ++yo) {
									byte c = px[((y + yo) * bd.Stride) + (x * s) + 1];
									color |= c != TRANSPARENCY;

									if (tilestart == 0) break;
								}
							}

							int height = -1;

							if (color) {
								if(tilestart == 0) {
									tilestart = y;

								} else if(y - tilestart == 24) {
									// height: 4 tiles
									height = 4;
								}

							} else if(tilestart > 0) {
								height = (y - tilestart) / 8;
							}

							// add new tile data
							if(height != -1) {
								if (height == 0) {
									height = 1;
								}
								// get tile colors
								List<byte> data = new List<byte>(32 * height * 8);

								for (int yy = tilestart; yy < y + 8; yy++) {
									for (int xx = rows[r] - (width - 1); xx <= rows[r]; xx += 2) {
										// collect colors
										byte x = 0;
										if(yy < bd.Height)
											x = (byte) ((getCol(px[(yy * bd.Stride) + (xx * s) + 1]) << 4) | getCol(px[(yy * bd.Stride) + ((xx + 1) * s) + 1]));
										data.Add(x);
									}
								}

								tilec += height * 2;
								sprites.Add(new Sprite(height, rows[r] - (width - 1), tilestart, data.ToArray()));
								tilestart = 0;
								y += 7;
							}
						}
					}
				}

				b.UnlockBits(bd);
				b.Dispose();

				string fname = png.Substring(png.LastIndexOf("\\") + 1);
				fname = fname.Substring(0, fname.IndexOf(" "));

				// save tiles
				string fn = @"lyrics\data\"+ fname +" tiles.dat";
				if (File.Exists(fn)) File.Delete(fn);

				using (var f = File.OpenWrite(fn)) {
					foreach(Sprite s in sprites) {
						byte[] d = s.getData();
						f.Write(d, 0, d.Length);
					}				
	
					f.Flush();
					f.Close();
				}

				// save sprites
				fn = @"lyrics\data\" + fname + " sprites.dat";
				if (File.Exists(fn)) File.Delete(fn);

				int _vram = vram;
				byte link = 0;

				using (var f = File.OpenWrite(fn)) {
					foreach (Sprite s in sprites) {
						byte[] d = s.getSprite(ref _vram, ref link, 0);
						f.Write(d, 0, d.Length);
					}

					// need to have an extra null byte
					f.Write(new byte[8], 0, 8);
					f.Flush();
					f.Close();
				}

				// save sprites 2
				fn = @"lyrics\data\" + fname + " sprites2.dat";
				if (File.Exists(fn)) File.Delete(fn);

				int _l = 320, _r = 0;

				foreach(Sprite s in sprites) {
					if (_l > s.x) _l = s.x;
					if (_r < s.x + s.width * width) _r = s.x + s.width * width;
				}

				_vram = vram;
				link = 0;

				using (var f = File.OpenWrite(fn)) {
					foreach (Sprite s in sprites) {
						byte[] d = s.getSprite(ref _vram, ref link, _r - _l);
						f.Write(d, 0, d.Length);
					}

					// need to have an extra null byte
					f.Write(new byte[8], 0, 8);
					f.Flush();
					f.Close();
				}

				// get num of tiles
				maxtile += tilec;
				Console.WriteLine(tilec + " tiles "+ sprites.Count + " sprites for " + png);
			}

			sw.Stop();
			Console.WriteLine("done in " + (sw.ElapsedMilliseconds / 1000f) + "s with "+ maxtile +" tiles at worst");
			Console.ReadKey();
		}
	}
}
