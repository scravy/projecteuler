package de.scravy.euler;

import java.math.BigInteger;

public class Euler26 implements Euler {

  private int lengthOfPeriod(int n) {
    int s = 10;
    int[] rs = new int[n];
    for (int i = 0; i < rs.length; i += 1) {
      for (int j = 0; j < i; j += 1) {
        if (rs[j] == s) {
          return i - j;
        }
      }
      rs[i] = s;
      s = (s % n) * 10;
      if (s == 0) {
        return 0;
      }
    }
    throw new AssertionError("can't happen");
  }

  @Override
  public BigInteger run() {
    int argmax = 2;
    int max = 0;
    for (int i = 2; i < 1000; i += 1) {
      final int newmax = lengthOfPeriod(i);
      if (newmax > max) {
        argmax = i;
        max = newmax;
      }
    }
    return BigInteger.valueOf(argmax);
  }
}
