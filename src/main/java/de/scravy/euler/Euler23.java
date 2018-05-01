package de.scravy.euler;

import java.math.BigInteger;
import java.util.function.IntConsumer;

public class Euler23 implements Euler {

  @Override
  public BigInteger run() {
    getPrimeFactors(12).forEach((IntConsumer) System.out::println);
    return BigInteger.ZERO;
  }
}
