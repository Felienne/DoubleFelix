using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;

namespace FormApp
{
   public partial class Form1 : Form
   {
      public Form1()
      {
         InitializeComponent();
      }

      private void button1_Click(object sender, EventArgs e)
      {
         var codeString = textBox1.Text;
         var result = DoubleFelix.EvalThatShit.Eval(codeString);
         textBox2.Text = result;
      }

      private void button2_Click(object sender, EventArgs e)
      {
         var goal = "HELLO FSHARP LOVERS";        
         
         int i = 0;
         while (!textBox1.Text.Contains(goal))
         {
            var codeString = textBox1.Text;
            var newCode = DoubleFelix.Mutate.mutateFunction(codeString);

            //are we on the right path?
            var levOld = DoubleFelix.Mutate.levenshtein(DoubleFelix.Mutate.getOutputString(codeString), goal);
            var levNew = DoubleFelix.Mutate.levenshtein(DoubleFelix.Mutate.getOutputString(newCode), goal);

            if (levOld.Item1 >= levNew.Item1)
            {
               i++;
               textBox3.Text = i.ToString();
               textBox1.Text = newCode;
               textBox1.Update();
               textBox3.Update();
            }
            else
            {
               textBox1.Text = codeString;
            }
      }



   }
   }
}
