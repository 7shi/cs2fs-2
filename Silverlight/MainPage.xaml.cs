using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;
using CSharpParser;

namespace Silverlight
{
    public partial class MainPage : UserControl
    {
        public MainPage()
        {
            InitializeComponent();
            Test("New.cs");
        }

        private void Translate()
        {
            Debug.Stream = new StringWriter();
            try
            {
                var lex = new Lexer(textBox1.Text);
                var tokens = lex.ReadAllTokens();
                var conv = new Converter(tokens);
                conv.Convert();
            }
            catch (Exception ex)
            {
                Debug.WriteLine();
                Debug.WriteLine(ex.Message);
            }
            Debug.Stream.Close();
            textBox2.Text = Debug.Stream.ToString();
        }

        private void Test(string src)
        {
            var uri = new Uri(src, UriKind.Relative);
            var stream = Application.GetResourceStream(uri).Stream;
            using (var sr = new StreamReader(stream))
                textBox1.Text = sr.ReadToEnd();
            button1_Click(null, null);
        }

        private void button1_Click(object sender, RoutedEventArgs e)
        {
            Translate();
        }

        private void btnNew_Click(object sender, RoutedEventArgs e)
        {
            Test("New.cs");
        }

        private void btnTest1_Click(object sender, RoutedEventArgs e)
        {
            Test("Test1.cs");
        }

        private void btnTest2_Click(object sender, RoutedEventArgs e)
        {
            Test("Converter.cs");
        }
    }
}
