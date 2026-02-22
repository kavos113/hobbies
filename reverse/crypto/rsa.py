from Crypto.Util.number import bytes_to_long, long_to_bytes, getPrime

message = "welcome to CTF!"
message_long = bytes_to_long(message.encode())

p = getPrime(1024)
q = getPrime(1024)
n = p * q
e = 65537
phi = (p - 1) * (q - 1)
d = pow(e, -1, phi)

ciphertext = pow(message_long, e, n)

print("Ciphertext:", ciphertext)
print("Public Key (n, e):", (n, e))