import base64
import glob

def to_ocaml(name, b64_string):
	return """open Utils.B64_decode

let %s_b64 = \"%s\"

let %s_rom () = decode_base64_to_bytes %s_b64""" % (name, b64_string.decode(), name, name)

for file in glob.glob("*.bin"):
	with open(file, "rb") as boot_rom:
		name = file[:file.index(".bin")]
		with open(name + ".ml", "w") as outfile:
			outfile.write(to_ocaml(name, base64.b64encode(boot_rom.read())))

