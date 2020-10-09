using System;
using System.Diagnostics;

namespace SyntacticalAnalyzer
{
  internal class Tests
  {
    public static void RunTests()
    {
      TestCorrectSingleLine();
      TestCorrectMultiLine();

      TestIncorrectSingleLine();
      TestIncorrectMultiLine();

      Console.WriteLine("All tests passed!");
    }

    private static void TestCorrect(string relation)
    {
      Debug.Assert(Parser.Parse(relation).Success, $"Test had to pass, but didn't: {relation}");
    }

    private static void TestIncorrect(string relation) 
    {
      Debug.Assert(!Parser.Parse(relation).Success, $"Test had to fail, but didn't: {relation}");
    }

    private static void TestCorrectSingleLine()
    {
      TestCorrect("hello :- f; g; f, g, f, g, f, g, ggg, g, ggg, fgfgfg, fg.");
      TestCorrect("f :- g.");

      TestCorrect("f.");
      TestCorrect("heloelelele12121wesdjashdasjdg22371236           .");
      TestCorrect("_sdhaskdha23123.");

      TestCorrect("hello :- my, name; (is ; Kir, ill)   .");
      TestCorrect("h :- h, h, h,   h,   hhhh, hhhh; hhhhhhhhh     ;  hhhh, shdgsajdhgasjhdg.");
      TestCorrect("w123:-a.");

      TestCorrect("f:- f, (f; (f, g, (f; a)), a).");

      TestCorrect("f :- f. f.");
      TestCorrect("f. f. f. f. f. hhe. helo :- helo.");
    }

    private static void TestCorrectMultiLine()
    {
      TestCorrect("f.\nf.\nf.");
      TestCorrect("f :-\n   g, (g, \n g; f, (f))\n.");
      TestCorrect("hello \n . hello :- f, g; \n (g, f, \n (g, f, \n g))\n.\n\n");
    }

    private static void TestIncorrectSingleLine()
    {
      TestIncorrect(":-");
      TestIncorrect("f :-");
      TestIncorrect(":- hello");
      TestIncorrect("f :- f");

      TestIncorrect("f :- g; h, .");
      TestIncorrect("f123 := f.");
      TestIncorrect("hmmm");

      TestIncorrect(":-:-:-:-");
      TestIncorrect("F :- g,.");
      TestIncorrect("f :- f...");

      TestIncorrect("fft :- f, (gl; ).");

      TestIncorrect("fft :- f, (f, g.");
      TestIncorrect("fft   :- f, (), f.");
      TestIncorrect("fft :- f, (f. f), a.");
    }

    private static void TestIncorrectMultiLine()
    {
      TestIncorrect("f. \n f. \n f \n f.");
      TestIncorrect("f. f. f. \n\n\n f\n. h :- \n .");
      TestIncorrect("hey :- h\n . h -: \n");
      TestIncorrect("f;\n f:- f.\n f.");
      TestIncorrect("f .\n . \n . f :- r.");
      TestIncorrect("f :- ; \n , f.");
    }
  }
}