using System;
using System.Diagnostics;

namespace RegexpParser
{
  class Program
  {
    static void Main(string[] args)
    {
      if (args.Length > 0 && args[0].ToLower() == "tests")
      {
        Tests.Run();
        return;
      }
      var regexp = Console.ReadLine();
      var line = Console.ReadLine();

      var watches = Stopwatch.StartNew();

      Console.WriteLine("{0}\n\nElapsed time: {1} ms", Matcher.Match(regexp, line), watches.ElapsedMilliseconds);
    }
  }
}
