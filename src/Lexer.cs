using System.IO;

namespace SyntacticalAnalyzer
{
  internal class Lexer
  {
    private TextReader myTextReader;

    public Lexer(TextReader textReader)
    {
      myTextReader = textReader;
      NextChar();
      NextLexema();
    }

    public Lexema CurrentLexema { get; private set; }

    public int CurrentLine
    {
      get 
      {
        if (myCurrentColon == -1) 
          return myCurrentLine - 1;
        else
          return myCurrentLine;
      }
    }

    public int CurrentColon
    {
      get 
      {
        if (myCurrentColon == -1) 
          return myPreviousColon;
        else
          return myCurrentColon;
      }
    }

    private char myCurrentChar;

    private int myCurrentLine;
    private int myCurrentColon;
    private int myPreviousColon;

    public void NextLexema()
    {
      if (CurrentLexema == Lexema.Failed)
        return;

      while (char.IsWhiteSpace(myCurrentChar))
        NextChar();

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
        case '.':
          NextChar();
          CurrentLexema = Lexema.Dot;
          return;
        case ',':
          NextChar();
          CurrentLexema = Lexema.Comma;
          return;
        case ';':
          NextChar();
          CurrentLexema = Lexema.Semicolon;
          return;
      }

      if (myCurrentChar == ':') 
      {
        NextChar();
        if (myCurrentChar == '-')
        {
          NextChar();
          CurrentLexema = Lexema.Turnstile;
        }
        else
        {
          CurrentLexema = Lexema.Failed;
        }
      }
      else if (char.IsLetter(myCurrentChar) || myCurrentChar == '_')
      {
        while (char.IsLetter(myCurrentChar) || char.IsDigit(myCurrentChar) || myCurrentChar == '_') 
          NextChar();

        CurrentLexema = Lexema.Identifier;
      } 
      else 
      {
        CurrentLexema = Lexema.Failed;
      }
    }

    private void NextChar()
    {
      int symbol = myTextReader.Read();
      
      if (symbol < 0)
        myCurrentChar = '\0';
      else
      {
        myCurrentChar = (char) symbol;
        myCurrentColon++;
      }

      if (myCurrentChar == '\n')
      {
        myCurrentLine++;
        myPreviousColon = CurrentColon;
        myCurrentColon = -1;
      }
    }
  }
}