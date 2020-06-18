package de.scravy.euler;

import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

public class Euler14 implements Euler {

  @Override
  public BigInteger run() {

    final Map<BigInteger, BigInteger> lengths = new HashMap<>();
    final BigInteger two = BigInteger.valueOf(2);
    final BigInteger three = BigInteger.valueOf(3);

    BigInteger max = BigInteger.ZERO;
    BigInteger maxSteps = BigInteger.ZERO;

    for (int i = 1; i <= 1000000; i += 1) {
      final BigInteger n = BigInteger.valueOf(i);
      BigInteger c = n;
      BigInteger steps = BigInteger.ONE;
      while (!c.equals(BigInteger.ONE)) {
        if (c.mod(two).equals(BigInteger.ZERO)) {
          c = c.divide(two);
        } else {
          c = c.multiply(three).add(BigInteger.ONE);
        }
        if (lengths.containsKey(c)) {
          steps = steps.add(lengths.get(c));
          break;
        }
        steps = steps.add(BigInteger.ONE);
      }
      lengths.put(n, steps);

      if (maxSteps.compareTo(steps) < 0) {
        maxSteps = steps;
        max = n;
      }
    }

    return max;
  }
}

