package de.scravy.euler;

import com.google.common.io.ByteStreams;
import com.simplaex.bedrock.Try;
import it.unimi.dsi.fastutil.ints.IntArrayList;
import org.reflections.Reflections;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.reflect.Constructor;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.Collectors;

@FunctionalInterface
public interface Euler {

  BigInteger run();

  static void print(final Object obj) {
    System.out.print(obj);
  }

  static void println(final Object obj) {
    System.out.println(obj);
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
          System.out.println("Failed to read the number of the problem to compulte the solution to.");
        },
        n -> {
          final Class<? extends Euler> clazz = solutionsById.get(n);
          if (clazz == null) {
            println("No solution to problem " + n);
          } else {
            final Euler solution = Try.execute(clazz::newInstance).get();
            System.out.println(solution.run());
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

  default IntArrayList getPrimeFactors(final int n) {
    return Primes.get().getPrimeFactors(n);
  }

}
