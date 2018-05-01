package de.scravy.euler;

import com.google.common.math.BigIntegerMath;
import com.google.common.math.IntMath;
import com.google.common.math.LongMath;
import com.simplaex.bedrock.NoOp;
import com.simplaex.bedrock.Try;
import it.unimi.dsi.fastutil.ints.IntArrayList;

import java.io.*;
import java.math.BigInteger;
import java.math.RoundingMode;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Consumer;
import java.util.function.IntConsumer;
import java.util.function.LongConsumer;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

public class Primes {

  private static BigInteger[] generate(final int howMany, final Consumer<BigInteger> handler) {

    final BigInteger[] ps = new BigInteger[howMany];

    final BigInteger TWO = BigInteger.valueOf(2L);
    ps[0] = TWO;
    int px = 1;
    handler.accept(ps[0]);
    BigInteger r = BigInteger.ONE;

    loop:
    while (px < ps.length) {
      r = r.add(TWO);
      final BigInteger s = BigIntegerMath.sqrt(r, RoundingMode.DOWN);
      for (int i = 0; i < px && ps[i].compareTo(s) <= 0; i += 1) {
        final BigInteger m = r.mod(ps[i]);
        if (m.equals(BigInteger.ZERO)) {
          continue loop;
        }
      }
      ps[px] = r;
      px += 1;
      handler.accept(r);
    }
    return ps;
  }

  private static long[] generate(final int howMany, final LongConsumer handler) {

    final long[] ps = new long[howMany];

    ps[0] = 2L;
    int px = 1;
    handler.accept(ps[0]);
    long r = 1L;

    loop:
    while (px < ps.length) {
      r += 2L;
      final long s = LongMath.sqrt(r, RoundingMode.DOWN);
      for (int i = 0; i < px && ps[i] <= s; i += 1) {
        final long m = r % ps[i];
        if (m == 0) {
          continue loop;
        }
      }
      ps[px] = r;
      px += 1;
      handler.accept(r);
    }
    return ps;
  }

  private static int[] generate(final int howMany, final IntConsumer handler) {

    final int[] ps = new int[howMany];

    ps[0] = 2;
    int px = 1;
    handler.accept(ps[0]);
    int r = 1;

    loop:
    while (px < ps.length) {
      r += 2;
      final int s = IntMath.sqrt(r, RoundingMode.DOWN);
      for (int i = 0; i < px && ps[i] <= s; i += 1) {
        final int m = r % ps[i];
        if (m == 0) {
          continue loop;
        }
      }
      ps[px] = r;
      px += 1;
      handler.accept(r);
    }
    return ps;
  }

  private static volatile Primes INSTANCE = null;

  public static Primes get() {
    if (INSTANCE == null) {
      synchronized (Primes.class) {
        if (INSTANCE == null) {
          INSTANCE = new Primes();
        }
      }
    }
    return INSTANCE;
  }

  private final int[] primes;

  private Primes() {
    primes = readOrGeneratePrimes();
  }

  private int[] readOrGeneratePrimes() {
    final int amount = 10_000_000;
    final Path path = Paths.get("primes.bin.gz");
    try {
      final int[] values = new int[amount];
      try (final FileInputStream fs = new FileInputStream(path.toFile());
           final GZIPInputStream zs = new GZIPInputStream(fs);
           final BufferedInputStream bs = new BufferedInputStream(zs);
           final DataInputStream ds = new DataInputStream(bs)) {
        for (int i = 0; i < amount; i += 1) {
          final int value = ds.readInt();
          values[i] = value;
        }
      } catch (final EOFException ignore) {
      }
      return values;
    } catch (final IOException ignore) {
      try {
        try (final FileOutputStream fs = new FileOutputStream(path.toFile());
             final GZIPOutputStream zs = new GZIPOutputStream(fs);
             final BufferedOutputStream bs = new BufferedOutputStream(zs);
             final DataOutputStream ds = new DataOutputStream(bs)) {
          return generate(amount, (IntConsumer) value -> {
            try {
              ds.writeInt(value);
            } catch (final IOException exc) {
              throw new RuntimeException(exc);
            }
          });
        }
      } catch (final IOException exc) {
        return generate(amount, (IntConsumer) NoOp.consumer());
      }
    }
  }

  public IntArrayList getPrimeFactors(final int n) {

    final IntArrayList list = new IntArrayList();

    int r = n;
    int i = 0;

    while (r > 1 && i < primes.length) {
      if (r % primes[i] == 0) {
        list.add(primes[i]);
        r = r / primes[i];
      } else {
        i += 1;
      }
    }

    return list;
  }

}
