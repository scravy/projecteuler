package de.scravy.euler;

import java.math.BigInteger;

public class Euler28 implements Euler {

  @Override
  public BigInteger run() {
    final int size = 1001;
    int currentGap = 2;
    int currentNum = 1;
    int sum = 1;
    for (int i = 0; i < size / 2; i += 1) {
      for (int j = 0; j < 4; j += 1) {
        currentNum += currentGap;
        sum += currentNum;
      }
      currentGap += 2;
    }
    return BigInteger.valueOf(sum);
  }
}
