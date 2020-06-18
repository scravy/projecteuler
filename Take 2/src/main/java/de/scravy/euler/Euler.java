package de.scravy.euler;

import com.google.common.io.ByteStreams;
import com.simplaex.bedrock.Try;
import de.scravy.primes.Primes;
import it.unimi.dsi.fastutil.ints.IntAVLTreeSet;
import it.unimi.dsi.fastutil.ints.IntSet;
import org.reflections.Reflections;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.stream.Collectors;

@SuppressWarnings("CodeBlock2Expr")
@FunctionalInterface
public interface Euler {

  BigInteger run();

  default void print(final Object obj) {
    System.out.print(obj);
  }

  default void println(final Object obj) {
    System.out.println(obj);
  }

  default void printf(final String format, final Object... args) {
    System.out.printf(format, args);
  }

  default IntSet properDivisors(final int n) {
    final int threshold = Primes.isqrt(n);
    final IntAVLTreeSet divisors = new IntAVLTreeSet();
    divisors.add(1);
    for (int i = 2; i <= threshold; i += 1) {
      if (n % i == 0) {
        divisors.add(i);
        divisors.add(n / i);
      }
    }
    return divisors;
  }

  default IntSet divisors(final int n) {
    final IntSet pd = properDivisors(n);
    pd.add(n);
    return pd;
  }

  enum PAD {
    Perfect,
    Abundant,
    Deficient
  }

  default PAD classifyPAD(final int n) {
    final IntSet pd = properDivisors(n);
    final int s = pd.stream().mapToInt(x -> x).sum();
    if (s == n) {
      return PAD.Perfect;
    } else if (s > n) {
      return PAD.Abundant;
    }
    return PAD.Deficient;
  }

  static void main(final String... args) {

    final Set<Class<? extends Euler>> solutions =
      new Reflections("de.scravy.euler").getSubTypesOf(Euler.class);

    final Map<Integer, Class<? extends Euler>> solutionsById =
      solutions.stream().collect(Collectors.toMap(
        clazz -> Integer.parseInt(clazz.getSimpleName().substring(5)),
        clazz -> clazz
      ));

    Try
      .execute(() -> Integer.parseInt(args[0]))
      .recover(
        exc -> {
          System.out.println("Solution to which problem would you like to compute?");
          final Scanner scanner = new Scanner(new BufferedReader(new InputStreamReader(System.in)));
          return scanner.nextInt();
        })
      .consume(
        exc -> {
          System.out.println("Failed to read the number of the problem to compute the solution to.");
        },
        n -> {
          final Class<? extends Euler> clazz = solutionsById.get(n);
          if (clazz == null) {
            System.out.println("No solution to problem " + n);
          } else {
            final Euler solution = Try.execute(clazz::newInstance).get();
            solution.println(solution.run());
          }
        }
      );
  }

  static <T extends Euler> void run(final Class<T> clazz) {
    Try
      .execute(clazz::getConstructor)
      .map(Constructor::newInstance)
      .fallbackWith(() -> {
        try {
          final Constructor<T> constructor = clazz.getConstructor(String.class);
          final InputStream inputStream = clazz.getResourceAsStream(clazz.getSimpleName());
          final byte[] bytes = ByteStreams.toByteArray(inputStream);
          final String string = new String(bytes, StandardCharsets.UTF_8);
          return constructor.newInstance(string);
        } catch (final Exception exc) {
          throw new RuntimeException(exc);
        }
      })
      .consume(exc -> {
        throw new RuntimeException(exc);
      }, instance -> {
        System.out.println(instance.run());
      });
  }

}
