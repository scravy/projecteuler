package de.scravy.euler;

import java.math.BigInteger;

public class Euler48 implements Euler {

  @Override
  public BigInteger run() {

    long s = 0;
    for (int i = 1; i <= 1000; i += 1) {
      long p = i;
      for (int j = 1; j < i; j += 1) {
        p *= i;
        p %= 10_000_000_000L;
      }
      s += p;
      s %= 10_000_000_000L;
    }

    return BigInteger.valueOf(s);
  }

}
