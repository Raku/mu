using System;
using System.Collections.Generic;
using System.Collections;
using System.Linq;
using System.IO;
using System.Extensions;
using IronPerl.Yaml;

namespace IronPerl {

    public class AST {

        int indent_level = 0;
        bool in_stmtlist = false;
        
        public static void Parse (string source) {
            
            string yaml = Get_STD_parse_tree(@"say 3");

            Parser parser = new Parser(new Scanner(new StringReader(yaml)), new Version(1, 0));
            Composer composer = new Composer(parser);

            if (!composer.CheckNode()) {
                throw new ConstructorException("Invalid YAML element: " + yaml);
            }
            Interpreter ip = new Interpreter();

            parser.Take(200000).ForEach(e => ip.Handle_YamlEvent(ref e));
            
        }

        static string indent(int indent) {
            return "\t".Repeat(indent).Join("");
        }

        static void logmsg(string msg, int depth) {
            Console.WriteLine(indent(depth) + msg);
        }

        void Handle_YamlEvent(ref YamlEvent ye) {

            string et = ye.GetType().ToString();
            /*Console.WriteLine("\nYamlEvent type is: " + et);
            Console.WriteLine("\ttext is:\n\t\t" + ye.ToString());*/

            string ename = ye.ToString();

            // parse the tag name...
            string tagname = "";

            // stupid exception catching
            try {
                tagname = ename.Matches(
                    //@"(\w+)"
                        @"tag:perl.yaml.org,2002:hash:VAST::(\w+)"""
                    ).Get1().Groups[1].ToString();
            } catch (Exception ex) {
                try {
                    //Console.Write("trying 2nd");
                tagname = ename.Matches(
                        @"Value=""([^""]+)"""
                    ).Get1().Groups[1].ToString();
                    //Console.WriteLine(" found " + tagname);
                } catch (Exception ex2) {
                    //Console.WriteLine(" didn't match 2nd");
                    // hee hee
                }
            }
            
            //  branch based on the type of Yaml Event.
            
            switch( et ) {
                case typeof(Yaml.AliasEvent).ToString() :
                    // ignore, hopefully! :)
                    break;
                case typeof(Yaml.CollectionEndEvent).ToString() :
                    break;
                case typeof(Yaml.CollectionStartEvent).ToString() :
                    break;
                case typeof(Yaml.DocumentEndEvent).ToString() :
                    // ignore
                    break;
                case typeof(Yaml.DocumentStartEvent).ToString() :
                    // ignore
                    break;
                case typeof(Yaml.MappingEndEvent).ToString() :
                    logmsg("end mapping: " + (
                        !tagname.IsNullOrEmpty() ? tagname : ""
                    ), --indent_level);
                    break;
                case typeof(Yaml.MappingStartEvent).ToString() :
                    logmsg("new mapping: " + (
                        !tagname.IsNullOrEmpty() ? tagname : ""
                    ), indent_level++);
                    break;
                case typeof(Yaml.ScalarEvent).ToString() :
                    break;
                case typeof(Yaml.SequenceEndEvent).ToString() :
                    break;
                case typeof(Yaml.SequenceStartEvent).ToString() :
                    break;
                case typeof(Yaml.StreamEndEvent).ToString() :
                    // ignore
                    break;
                case typeof(Yaml.StreamStartEvent).ToString() :
                    // ignore
                    break;
            }

        }



        private static void RunPerl() {




            //TestMath.RunIt();
        }

        /// <summary>
        ///     Send a Perl 6 source string to STD.pm; return the parse tree (in YAML?)
        /// </summary>
        /// <param name="src">string holding Perl 6 source</param>
        /// <returns>string holding YAML parse tree</returns>
        public static string Get_STD_parse_tree(string src) {
            Console.WriteLine("calling out to STD");
            string tmpfilename = Path.GetTempFileName();
            File.WriteAllText(tmpfilename, src);
            string stdout = Cmd_Line_App(@"c:\cygwin\cygwinSTD.bat", new string[] { tmpfilename });
            File.Delete(tmpfilename);
            return stdout;
        }


        /// <summary>
        ///     Execute a command-line application, capturing any stdout.
        ///     TODO: also capture stderr; optionally return them as streams;
        ///     also capture program exit code.
        /// </summary>
        /// <param name="args"></param>
        /// <returns></returns>
        public static string Cmd_Line_App(string cmd, string[] args) {
            Process p = new Process();
            p.StartInfo.UseShellExecute = false;
            p.StartInfo.RedirectStandardOutput = true;
            p.StartInfo.FileName = cmd;
            p.StartInfo.Arguments = String.Join(" ", args);
            /*if (args.Length > 0)
                p.StartInfo.Arguments = String.Join(" ",
                    ((IEnumerable<string>)new List<string>(args)).Select<string, string>(x =>
                    { // All these escapings are necessary b/c this is hitting a 
                        //     .bat file as %* then hitting an .sh script as $1
                        return "\"" + x.Replace("\"", "\"\"").Replace("\\", "\\\\\\\\") + "\"";
                    }).ToArray()); */
            p.Start();
            string output = p.StandardOutput.ReadToEnd();
            p.WaitForExit();
            return output;
        }
    }
}