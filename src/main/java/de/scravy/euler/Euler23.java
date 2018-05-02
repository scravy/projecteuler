package de.scravy.euler;

import it.unimi.dsi.fastutil.ints.IntArrayList;
import it.unimi.dsi.fastutil.ints.IntList;

import java.math.BigInteger;
import java.util.BitSet;

public class Euler23 implements Euler {

  @Override
  public BigInteger run() {
    final int upto = 28123;
    final IntList abundantNumbers = new IntArrayList();
    final BitSet canBeExpressedAsSumOfAbundantNumbers = new BitSet(upto + 1);
    for (int i = 1; i <= upto; i += 1) {
      if (classifyPAD(i) == PAD.Abundant) {
        abundantNumbers.add(i);
      }
    }
    for (int i = 0; i < abundantNumbers.size(); i += 1) {
      for (int j = 0; j <= i; j += 1) {
        final int n = abundantNumbers.getInt(i) + abundantNumbers.getInt(j);
        canBeExpressedAsSumOfAbundantNumbers.set(n);
      }
    }
    int sum = 0;
    for (int i = 1; i <= upto; i += 1) {
      if (!canBeExpressedAsSumOfAbundantNumbers.get(i)) {
        sum += i;
      }
    }
    return BigInteger.valueOf(sum);
  }
}
