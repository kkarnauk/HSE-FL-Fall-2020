using System;
using System.IO;
using System.Text;

namespace SyntacticalAnalyzer
{
  public class Program
  {
    public static void Main(string[] args)
    {
      if (args.Length > 0 && args[0].ToLower() == "tests")
      {
        Tests.RunTests();

        return;
      }

      string input;

      if (args.Length > 0 && args[0].ToLower() == "file")
      {
        if (args.Length < 2)
        {
          Console.WriteLine("Please, pass file name.");

          return;
        }

        var currentPath = Directory.GetCurrentDirectory();

        input = File.ReadAllText(currentPath + "/" + args[1]);
      }
      else
      {
        input = ReadConsole();
      }

      var parserResult = Parser.Parse(input);

      if (parserResult.Success)
        Console.WriteLine("Program is correct.");
      else
      {
        Console.WriteLine("Program is not correct.\nSyntax error: {0} line, {1} colon.", 
          parserResult.ErrorLine + 1, parserResult.ErrorColon);      
      }
    }

    private static string ReadConsole()
    {
      var currentText = new StringBuilder();

      string line;
      while ((line = Console.ReadLine()) != null)
        currentText.Append(line + "\n");

      return currentText.ToString();
    }
  }
}
