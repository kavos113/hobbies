def ksa(key):
    key_length = len(key)
    S = list(range(256)) # 0, 1, 2, ..., 255
    j = 0

    for i in range(256):
        j = (j + S[i] + key[i % key_length]) % 256
        S[i], S[j] = S[j], S[i] 

    return S

def process(S, plaintext):
    i = 0
    j = 0
    ciphertext = bytearray()

    for byte in plaintext:
        i = (i + 1) % 256
        j = (j + S[i]) % 256
        S[i], S[j] = S[j], S[i] 

        keystream_byte = S[(S[i] + S[j]) % 256]

        ciphertext.append(byte ^ keystream_byte)

    return bytes(ciphertext)

def encrypt(key, plaintext):
    S = ksa(key)
    return process(S, plaintext)

def decrypt(key, ciphertext):
    S = ksa(key)
    return process(S, ciphertext)

if __name__ == "__main__":
    key = b'secretkey'
    plaintext = b'Hello, World!'

    ciphertext = encrypt(key, plaintext)
    print(f"Ciphertext: {ciphertext.hex()}")    

    decrypted = decrypt(key, ciphertext)
    print(f"Decrypted: {decrypted.decode()}")