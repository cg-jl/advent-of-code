#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef unsigned char byte;

size_t read_bits(const byte *bits, size_t amt) {
  size_t r = 0;
  for (size_t i = 0; i < amt; ++i) {
    r <<= 1;
    r |= bits[i];
  }
  return r;
}

const byte *packet_calc(const byte *packet, size_t packet_size, size_t *result);

// operate through the inner operator packets. `cond` and `next` have access to
// the `i` value in the loops.
#define THROUGH_PACKETS(func, packet, packet_size, next, cond, op)             \
  {                                                                            \
    if (*packet++) {                                                           \
      size_t no_subpackets = read_bits(packet, 11);                            \
      packet += 11, packet_size -= 11;                                         \
      for (size_t i = 0; i < no_subpackets && (cond); ++i) {                   \
        const byte *next_packet = func(packet, packet_size, &next);            \
        op;                                                                    \
        packet_size -= (next_packet - packet);                                 \
        packet = next_packet;                                                  \
      }                                                                        \
    } else {                                                                   \
      size_t bits_left = read_bits(packet, 15);                                \
      packet += 15;                                                            \
      packet_size -= 15;                                                       \
      size_t i = 0;                                                            \
      do {                                                                     \
        const size_t consumed = func(packet, bits_left, &next) - packet;       \
        op;                                                                    \
        packet += consumed;                                                    \
        bits_left = bits_left > consumed ? bits_left - consumed : 0;           \
        i++;                                                                   \
      } while (bits_left && (cond));                                           \
    }                                                                          \
  }

const byte *packet_calc(const byte *packet, size_t packet_size,
                        size_t *result) {
  if (packet_size < 6)
    return packet;
  size_t version = read_bits(packet, 3);
  packet += 3, packet_size -= 3;
  size_t type_id = read_bits(packet, 3);
  packet += 3, packet_size -= 3;

  switch (type_id) {
  case 0: {
    // sum operator
    *result = 0;
    size_t next;
    THROUGH_PACKETS(packet_calc, packet, packet_size, next, true,
                    *result += next);
  } break;
  case 1: {
    // product operator
    *result = 1;
    size_t next;
    THROUGH_PACKETS(packet_calc, packet, packet_size, next, true,
                    *result *= next);
  } break;
  case 2: {
    // min operator
    *result = SIZE_MAX;
    size_t next;
    THROUGH_PACKETS(packet_calc, packet, packet_size, next, true,
                    *result = *result < next ? *result : next);
  } break;
  case 3: {
    // max operator
    *result = 0;
    size_t next;
    THROUGH_PACKETS(packet_calc, packet, packet_size, next, true,
                    *result = *result > next ? *result : next);
  } break;
  case 4: {
    *result = 0;
    do {
      *result = *result << 4 | read_bits(packet + 1, 4);
      packet += 5;
    } while ((packet - 5)[0]);
  } break;
  case 5: {
    size_t values[2];
    THROUGH_PACKETS(packet_calc, packet, packet_size, values[i], i < 2, {});
    *result = values[0] > values[1];
  } break;
  case 6: {
    size_t values[2];
    THROUGH_PACKETS(packet_calc, packet, packet_size, values[i], i < 2, {});
    *result = values[0] < values[1];
  } break;
  case 7: {
    size_t values[2];
    THROUGH_PACKETS(packet_calc, packet, packet_size, values[i], i < 2, {});
    *result = values[0] == values[1];
  } break;
  }
  return packet;
}

const byte *packet_version_sum(const byte *packet, size_t packet_size,
                               size_t *target) {
  size_t version = read_bits(packet, 3);
  packet += 3, packet_size -= 3;
  size_t type_id = read_bits(packet, 3);
  packet += 3, packet_size -= 3;

  if (type_id != 4) {
    size_t next;
    THROUGH_PACKETS(packet_version_sum, packet, packet_size, next, true,
                    version += next);
  } else {
    // process each 5-byte number
    while (packet[0])
      packet += 5;
    packet += 5;
  }
  *target = version; // probable cache miss?
  return packet;
}

int main(int argc, char const *const argv[]) {
  if (argc != 2) {
    fprintf(stderr, "usage: %s <file>\n", *argv);
    return 1;
  }
  FILE *const fp = fopen(argv[1], "rb");
  if (!fp) {
    perror("fopen");
    return 1;
  }
  fseek(fp, 0, SEEK_END);
  const size_t fsize = ftell(fp);
  const size_t packet_size = fsize * 4;

  byte *const packet = (byte *)malloc(packet_size);
  fseek(fp, 0, SEEK_SET);
  fread(packet, fsize, 1, fp);
  fclose(fp);
  printf("fsize = %lu\n", fsize);
  printf("packet_size = %lu\n", packet_size);

  for (size_t i = 0; i < fsize; ++i) {
    // make the conversion from ascii to hex
    // (branchless)
    packet[i] = packet[i] > '9' ? packet[i] - 'A' + 10 : packet[i] - '0';
  }

  // expand the bytes into their four bits
  for (size_t i = fsize; i > 0; --i) {
    size_t offset = 4 * i - 4;
    byte value = packet[i - 1];
    packet[offset + 3] = value & 1;
    value >>= 1;
    packet[offset + 2] = value & 1;
    value >>= 1;
    packet[offset + 1] = value & 1;
    value >>= 1;
    packet[offset + 0] = value & 1;
  }

  size_t s;
  packet_version_sum(packet, packet_size, &s);
  printf("total sum: %lu\n", s);

  packet_calc(packet, packet_size, &s);
  printf("value of packet: %lu\n", s);

  free(packet);

  return 0;
}
