using System;
using System.Text;
using System.IO;

namespace RegexpParser 
{
  internal class Lexer
  {
    private StringReader myStringReader;

    public Lexer(string expression) 
    {
      myStringReader = new StringReader(expression);
      NextChar();
      NextLexema();
    }

    public string CurrentString { get; private set; }

    public Lexema CurrentLexema { get; private set; }

    private char myCurrentChar;

    public void NextLexema() 
    {
      CurrentString = "";

      while (char.IsWhiteSpace(myCurrentChar))
        NextChar();

      // One-letter lexemas
      switch (myCurrentChar)
      {
        case '\0':
          CurrentLexema = Lexema.EOF;
          return;
        case '(':
          NextChar();
          CurrentLexema = Lexema.OpenBracket;
          return;
        case ')':
          NextChar();
          CurrentLexema = Lexema.CloseBracket;
          return;
      }

      // String
      if (myCurrentChar == '"' || myCurrentChar == '\'') 
      {
        var startSymbol = myCurrentChar;
        var readString = new StringBuilder();

        NextChar();

        while (myCurrentChar != startSymbol) 
        {
          if (myCurrentChar == '\0')
            throw new ArgumentException("No close quotation mark or apostrophe.");

          readString.Append(myCurrentChar);
          NextChar();
        }

        CurrentString = readString.ToString();
        CurrentLexema = Lexema.String;

        NextChar();
      }
      else // Reserved word
      {
        var readString = new StringBuilder();
        
        while (char.IsLetter(myCurrentChar))
        {
          readString.Append(myCurrentChar);
          NextChar();
        }

        switch (readString.ToString().ToLower()) 
        {
          case "empty":
            CurrentLexema = Lexema.Empty;
            return;
          case "epsilon":
            CurrentLexema = Lexema.Epsilon;
            return;
          case "seq":
            CurrentLexema = Lexema.Seq;
            return;
          case "alt":
            CurrentLexema = Lexema.Alt;
            return;
          case "star":
            CurrentLexema = Lexema.Star;
            return;
          default:
            throw new ArgumentException("Unknown reserved word \"" + readString.ToString() + "\"");
        }
      }
    }

    private void NextChar()
    {
      int symbol = myStringReader.Read();
      if (symbol < 0)
        myCurrentChar = '\0';
      else
        myCurrentChar = (char) symbol;
    }
  }
}