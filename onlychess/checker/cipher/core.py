from typing import List

from .constants import VECTOR
from .utils import add_vectors_mod_2


class Register:
    def __init__(self, length: int):
        self.state = [0] * length
    
    def get_state(self):
        return self.state

    def load_state(self, state: List[int]):
        if len(state) != len(self.state):
            raise ValueError(f"State length must be {len(self.state)}")
        self.state = state

    def shift_left(self):
        self.state = self.state[1:] + [0]


class LSFR_c(Register):
    def __init__(self):
        super().__init__(39)

    def feedback(self):
        # s[39+t] = s[37+t] + s[25+t] + s[24+t] + s[22+t] + s[8+t] + s[6+t] + s[4+t] + s[t]
        return (self.state[37] + self.state[25] + self.state[24]
                + self.state[22] + self.state[8] + self.state[6]
                + self.state[4] + self.state[0]) % 2

    def f_c(self):
        # c = 2*s[12] + s[20] + 1
        return 2*self.state[12] + self.state[20] + 1

    def clock(self):
        c = self.f_c()
        new_bit = self.feedback()
        self.shift_left()
        self.state[-1] = new_bit
        return c


class LSFR_d(Register):
    def __init__(self):
        super().__init__(89)

    def feedback(self):
        # u[89+t] = u[88+t] + u[50+t] + u[47+t] + u[36+t] + u[34+t] + u[9+t] + u[6+t] + u[t]
        return (self.state[88] + self.state[50] + self.state[47]
                + self.state[36] + self.state[34] + self.state[9]
                + self.state[6] + self.state[0]) % 2

    def f_d(self):
        # f_d(s[0], s[1], s[3], s[7], s[12], s[20], s[30], s[44], s[65], s[80])
        bits = (self.state[0], self.state[1], self.state[3], self.state[7],
                self.state[12], self.state[20], self.state[30], self.state[44],
                self.state[65], self.state[80])
        
        bits = tuple(bit for bit in bits[::-1])
        
        if len(bits) != 10 or any(b not in (0, 1) for b in bits):
            raise ValueError("Expected exactly 10 arguments, each equal to 0 or 1")

        # convert the bit sequence into an index (0‥1023)
        index = 0
        for b in bits:
            index = (index << 1) | b

        return VECTOR[index]
       
    def clock(self, c: int):
        z = self.f_d()
        
        for _ in range(c):
            new_bit = self.feedback()
            self.shift_left()
            self.state[-1] = new_bit
        return z


class StreamCipher:
    def __init__(self, secret_key: List[int]):
        assert len(secret_key) == 128, "Length key must be equal 128"
        
        self.secret_key = secret_key
        self.lsfr_c = LSFR_c()
        self.lsfr_d = LSFR_d()

    def crypt(self, plaintext: List[int], IV: List[int]):
        """encrypt or decrypt"""
        assert len(IV) == 128, "Length IV must be equal 128"

        ciphertext = []

        self.lsfr_c.load_state(add_vectors_mod_2(self.secret_key[:39], IV[:39]))
        self.lsfr_d.load_state(add_vectors_mod_2(self.secret_key[39:], IV[39:]))

        for el in plaintext:
            c = self.lsfr_c.clock()
            z = self.lsfr_d.clock(c)
            ciphertext.append((el + z) % 2)
        
        return (ciphertext, IV)