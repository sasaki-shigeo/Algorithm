# Base64 encode / decode

## 基本的アイディア
$$ 3\,\mathrm{bytes} = 24\,\mathrm{bits} $$
$$ 4\,\mathrm{chars} \times (6\,\mathrm{bits} / \mathrm{char}) = 24\,\mathrm{bits}$$
$$\therefore\;3\,\mathrm{bytes} = 4\,\mathrm{chars}$$

| encode/decode | conversion |
|---------------|------------|
| encode  | 3 bytes &rightarrow; 4 chars |
| decode  | 4 chars &rightarrow; 3 bytes |

## 使用する文字
| 文字  | 数 |
|------|----|
|[A-Z] | 26 |
|[a-z] | 26 |
|[0-9] | 10 |
|[+/]  |  2 |
|=     | padding |

## encoding

```
chars[0] =  (bytes[0] & 0xfc) >> 2;
chars[1] = ((bytes[0] & 0x03) << 4) |
           ((bytes[1] & 0xf0) >> 4);
chars[2] = ((bytes[1] & 0x0f) << 2) |
           ((bytes[2] & 0xc0) >> 6);
chars[3] =   bytes[2] & 0x3f;
```

## decoding

```
bytes[0] = (chars[0] << 2) | ((chars[1] & 0x30) >> 4);
bytes[1] = ((chars[1] & 0x0f) << 4) | ((chars[2] & 0x3c) >> 2);
bytes[2] = ((chars[2] & 0x03) << 6) | chars[3];
```