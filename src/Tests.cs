using System;
using System.Diagnostics;
using System.Linq;

namespace RegexpParser
{
  internal static class Tests
  {
    public static void Run()
    {
      BasicTests();
      EasyTests();
      HardTests();
      LongTimeTests();
    }

    private static void TestMatch(string regexp, string line) =>
      Debug.Assert(Matcher.Match(regexp, line));

    private static void TestMismatch(string regexp, string line) =>
      Debug.Assert(!Matcher.Match(regexp, line));

    private static string RepeatString(string str, int n)
    {
      return string.Concat(Enumerable.Repeat(str, n));
    }

    private static void BasicTests()
    {
      var watches = Stopwatch.StartNew();

      TestMatch("Seq 'a' 'b'", "ab");
      TestMismatch("Seq 'a' 'b'", "ba");

      TestMatch("Alt 'ab' \"b\"", "b");
      TestMatch("Alt 'pppp' 'q'", "pppp");
      TestMismatch("Alt 'd' 'e'", "c");

      TestMatch("Epsilon", "");
      TestMismatch("Epsilon", "hello");

      TestMismatch("Empty", "");

      TestMatch("'abcd'", "abcd");
      TestMismatch("'hello world'", "hello");

      TestMatch("Star 'hey'", "heyheyhey");
      TestMismatch("Star 'how'", "howhowhey");

      Console.WriteLine("Basic tests: {0} ms", watches.ElapsedMilliseconds);
    }

    private static void EasyTests()
    {
      var a100   = RepeatString("a", 100);
      var b100  = RepeatString("b", 100);
      var ab1000 = RepeatString("ab", 1000);

      var watches = Stopwatch.StartNew();

      TestMatch("Seq Alt 'a' 'b' 'abdc'", "aabdc");
      TestMatch("Seq (Alt 'bd' \"hello\") 'hey'", "hellohey");

      TestMatch("Star 'a'", a100);
      TestMismatch("Star 'a'", b100);

      TestMatch("Star Alt 'a' 'b'", a100 + b100 + ab1000);

      TestMismatch("Seq (Seq Epsilon Empty) 'abcd'", "abcd");

      Console.WriteLine("Easy tests: {0} ms", watches.ElapsedMilliseconds);
    }

    private static void HardTests()
    {
      var oho100 = RepeatString("oho", 1000);
      var ac1000 = RepeatString("ac", 1000);

      var watches = Stopwatch.StartNew();

      TestMatch("Seq (Alt 'hello' 'friend') (Star (Alt 'oho' 'hey'))", "hello" + oho100 + "hey" + oho100);

      TestMatch("Seq (Alt 'hey' 'oho') Seq (Alt 'wow' 'nope') Star 'a'", "heynopeaaaa");
      TestMismatch("Seq (Alt 'hey' 'oho') Seq (Alt 'wow' 'nope') Star 'a'", "heynopeb");

      TestMatch("Seq Epsilon Seq Epsilon Seq Epsilon Epsilon", "");
      TestMatch("Seq 'a' Seq Epsilon Seq 'b' Epsilon", "ab");

      TestMatch("Star Star Star Star 'oho'", oho100);

      TestMatch("Star (Seq 'a' Alt 'b' 'c')", ac1000 + "abacacac");
      TestMismatch("Star (Seq 'a' Alt 'b' 'cc')", "accabac");

      Console.WriteLine("Hard tests: {0} ms", watches.ElapsedMilliseconds);
    }

    private static void LongTimeTests()
    {
      var a1000000 = RepeatString("a", 10000000);
      var ab10000 = RepeatString("ab", 10000);
      var watches = Stopwatch.StartNew();

      TestMatch("Star 'a'", a1000000);
      TestMatch("Seq 'a' Star 'ab'", "a" + ab10000);
      TestMatch("Seq Star 'ab' 'ab'", ab10000);
      TestMismatch("Seq Star 'ab' 'b'", ab10000);

      Console.WriteLine("Long time tests: {0} ms", watches.ElapsedMilliseconds);
    }
  }
}