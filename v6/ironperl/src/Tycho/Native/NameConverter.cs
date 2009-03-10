using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Tycho.Runtime;
using System.Text.RegularExpressions;

namespace Tycho.Native {
    public class NameConverter {
        private static Regex UpperCaseLetters = new Regex (@"\p{Lu}+");
        private static Regex WordWord = new Regex (@"(\p{Ll})(\p{Lu}\p{Ll})");
        private static Regex AcronymWord = new Regex (@"(\p{Lu}+)(\p{Lu}\p{Ll})");
        private static Regex WordAcronym = new Regex (@"(\p{Ll})(\p{Lu}+)");

        public static string ConvertNamespace (string name) {
            return String.Join (":", name.Split ('.').Select (p => Convert (p)).ToArray ());
        }

        public static string Convert (string name) {
            return String.Join ("-", name.Split (new [] {'_'}, StringSplitOptions.RemoveEmptyEntries).Select (w => ConvertWord (w)).ToArray ());
        }

        public static string ConvertWord (string name) {
            char[] chars = name.ToCharArray ();
            StringBuilder result = new StringBuilder(name.Length * 2);

            char lastUpper = '\0';
            bool inUpperMode = true;
            bool isFirst = true;

            for (int i = 0; i < chars.Length; i++) {
                char c = chars[i];
                
                if (isFirst) {
                    result.Append (char.ToLowerInvariant (c));
                } else if (inUpperMode) {
                    if (char.IsLower (c)) {
                        if (lastUpper != '\0') {
                            result.Append ('-');
                            result.Append (lastUpper);
                            lastUpper = '\0';
                        }
                        result.Append (c);
                        inUpperMode = false;
                    } else if (char.IsUpper (c)) {
                        if (lastUpper != '\0') {
                            result.Append (lastUpper);
                        }
                        lastUpper = char.ToLowerInvariant (c);
                    } else {
                        result.Append (c);
                    }
                } else {
                    if (char.IsLower (c)) {
                        result.Append (c);
                    } else if (char.IsUpper (c)) {
                        result.Append ('-');
                        result.Append (char.ToLowerInvariant (c));
                        inUpperMode = true;
                    } else {
                        result.Append (c);
                    }
                }

                isFirst = false;
            }

            if (lastUpper != '\0') {
                result.Append (lastUpper);
            }

            return result.ToString ();
        }

        private static string ReplaceUpperCaseLetters (Match match) {
            string upperCaseLetters = match.Value.ToLowerInvariant ();
            int length = upperCaseLetters.Length;

            if (length > 1) {
                return '-' + upperCaseLetters.Substring (0, length - 1) + '-' + upperCaseLetters[length - 1];
            } else {
                return '-' + upperCaseLetters;
            }
        }

        public static string ConvertFromNamespace (Namespace ns) {
            if (ns.Parent != null) {
                return ConvertFromNamespace (ns.Parent) + "." + ns.Name;
            } else {
                return ns.Name;
            }
        }
    }
}
