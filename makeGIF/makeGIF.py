from PIL import Image
import glob

dirname = '../lattice/result'
filename = dirname + '/*.png'
files = sorted(glob.glob(filename))
images = list(map(lambda file: Image.open(file), files))

gifname = dirname + '.gif'
images[0].save(gifname, save_all=True, append_images=images[1:], duration=50, loop=0)
