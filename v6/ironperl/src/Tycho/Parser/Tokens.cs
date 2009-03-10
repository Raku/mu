using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Tycho.Parser.Tokens {
    public class SourceLocation {
        public string Source { get; private set; }
        public string FileName { get; private set; }
        public int LineStart { get; private set; }
        public int LineEnd { get; private set; }
        public int ColumnStart { get; private set; }
        public int ColumnEnd { get; private set; }

        public SourceLocation (string source, string fileName, int lineStart, int lineEnd, int columnStart, int columnEnd) {
            Source = source;
            FileName = fileName;
            LineStart = lineStart;
            LineEnd = lineEnd;
            ColumnStart = columnStart;
            ColumnEnd = columnEnd;
        }

        public SourceLocation RangeWith (SourceLocation sloc) {
            return new SourceLocation (Source, FileName, LineStart, sloc.LineEnd, ColumnStart, sloc.ColumnEnd);
        }
    }

    public enum BracketType {
        Brace,
        Parenthesis,
        Bracket
    }

    public class Token {
        public List<Token> Attributes { get; private set; }
        public SourceLocation SourceLocation { get; private set; }

        public Token (List<Token> attributes, SourceLocation sourceInformation) {
            Attributes = attributes;
            SourceLocation = sourceInformation;
        }
    }

    public class IntegerToken : Token {
        public int Value { get; private set; }

        public IntegerToken (List<Token> attributes, SourceLocation sourceInformation, int value)
            : base (attributes, sourceInformation) {
            Value = value;
        }
    }

    public class IdentifierToken : Token {
        public string Identifier { get; private set; }
        public List<string> ModulePath { get; set; }
        public bool IsOperator { get; private set; }

        public IdentifierToken (List<Token> attributes, SourceLocation sourceInformation, string identifier, List<string> modulePath, bool isOperator)
            : base (attributes, sourceInformation) {
            Identifier = identifier;
            ModulePath = modulePath;
            IsOperator = isOperator;
        }
    }

    public class StringToken : Token {
        public string Value { get; private set; }
        public string Label { get; private set; }
        public bool Escaped { get; private set; }

        public StringToken (List<Token> attributes, SourceLocation sourceInformation, string value, string label, bool escaped)
            : base (attributes, sourceInformation) {
            Value = value;
            Label = label;
            Escaped = escaped;
        }

        public string UnescapedValue {
            get {
                if (Escaped) {
                    char[] chars = Value.ToCharArray ();
                    StringBuilder result = new StringBuilder (Value.Length);

                    for (int i = 0; i < chars.Length; i++) {
                        char c = chars[i];

                        if (c == '\\' && i != chars.Length - 1) {
                            c = chars[++i];
                            switch (c) {
                                case 't':
                                    result.Append ('\t');
                                    break;
                                case 'n':
                                    result.Append ('\n');
                                    break;
                                case 'r':
                                    result.Append ('\r');
                                    break;
                                case '\\':
                                    result.Append ('\\');
                                    break;
                                default:
                                    result.Append (c);
                                    break;
                            }
                        } else {
                            result.Append (c);
                        }
                    }

                    return result.ToString ();
                } else {
                    return Value;
                }
            }
        }
    }

    public class FloatToken : Token {
        public double Value { get; set; }

        public FloatToken (List<Token> attributes, SourceLocation sourceInformation, double value)
            : base (attributes, sourceInformation) {
            Value = value;
        }
    }

    public class BracketToken : Token {
        public List<Token> Tokens { get; private set; }
        public BracketType BracketType { get; private set; }

        public BracketToken (List<Token> attributes, SourceLocation sourceInformation, List<Token> tokens, BracketType bracketType)
            : base (attributes, sourceInformation) {
            Tokens = tokens;
            BracketType = bracketType;
        }
    }

    public class Version {
        public int Major { get; private set; }
        public int Minor { get; private set; }
        public int Revision { get; private set; }
        public int Build { get; private set; }

        public Version (int major, int minor, int revision, int build) {
            Major = major;
            Minor = minor;
            Revision = revision;
            Build = build;
        }
    }

    public class VersionToken : Token {
        public Version Version { get; private set; }
        public Version ToVersion { get; private set; }
        public string Branch { get; private set; }

        public VersionToken (List<Token> attributes, SourceLocation sourceInformation, Version version, Version toVersion, string branch)
            : base (attributes, sourceInformation) {
            Version = version;
            ToVersion = toVersion;
            Branch = branch;
        }
    }

    public class ErrorToken : Token {
        public string Error { get; private set; }

        public ErrorToken (List<Token> attributes, SourceLocation sourceInformation, string error)
            : base (attributes, sourceInformation) {
            Error = error;
        }
    }
}
