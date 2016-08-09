using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.IO.Compression;
using System.Collections;
using System.Data;
namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            string scriptFile = @"E:\Script.R";//Script file path
            string header = @"library(igraph)
ComputeClusters <- function() 
{
  g<-graph.empty( directed = FALSE)

            //Writing Header
            File.AppendAllText(scriptFile, header + Environment.NewLine);
            
            string fileName = @"E:\nodes.txt"; //Vertices file path

            Console.WriteLine("Reading graph vertices...");
            using (FileStream fileToDecompress = File.Open(fileName, FileMode.Open))
            {

                StreamReader sr = new StreamReader(fileToDecompress);
                long lines = 0;
                long total = 1;
                StringBuilder readLine = new System.Text.StringBuilder();
                while (!sr.EndOfStream)
                {
                    try
                    {
                        if (lines == 0)
                        {
                            readLine.Append("g<- g+ vertices(");
                        }
                        if (lines < 5000)
                        {
                            readLine.Append("\"" + sr.ReadLine() + "\",");
                        }
                        else if (lines == 5000)
                        {
                            readLine.Append("\"" + sr.ReadLine() + "\")" + Environment.NewLine);
                            File.AppendAllText(scriptFile, readLine.ToString());
                            readLine.Clear();
                            Console.WriteLine("line #" + total);
                            total++;
                            lines = 0;
                            continue;
                        }

                        lines++;
                    }
                    catch { continue; }
                }
                if (readLine.ToString().Length > 0)//In case there are still less than the full package and file end is realised
                {
                    File.AppendAllText(scriptFile, readLine.ToString().Remove(readLine.Length - 1, 1) + ")"+ Environment.NewLine);
                    readLine.Clear();
                    Console.WriteLine("line #" + total);
                }
            }
			
            fileName = @"E:\Edges.txt";//Edges file path

            Console.WriteLine("Reading graph edges..." + fileName);

            using (FileStream fileToDecompress = File.Open(fileName, FileMode.Open))
            {
                StreamReader sr = new StreamReader(fileToDecompress);
                long lines = 0;
                long total = 1;
                StringBuilder readLine = new System.Text.StringBuilder();
                while (!sr.EndOfStream)
                {
                    try
                    {
                        if (lines == 0)
                        {
                            readLine.Append("g<- g+ edges(");
                        }
                        if (lines < 5000)
                        {
                            readLine.Append("\"" + sr.ReadLine().Replace(",", "\",\"") + "\",");
                        }
                        else if (lines == 5000)
                        {
                            readLine.Append("\"" + sr.ReadLine().Replace(",", "\",\"") + "\")" + Environment.NewLine);
                            File.AppendAllText(scriptFile, readLine.ToString());
                            readLine.Clear();
                            Console.WriteLine("line #" + total);
                            total++;
                            lines = 0;
                            continue;
                        }

                        lines++;
                    }
                    catch { continue; }
                }
                if (readLine.ToString().Length > 0)//In case there are still less than the full package and file end is realised
                {
                    File.AppendAllText(scriptFile, readLine.ToString().Remove(readLine.Length - 1, 1) + ")" + Environment.NewLine);
                    readLine.Clear();
                    Console.WriteLine("line #" + total);
                }
            }

            string footer = @"fc <- multilevel.community(g)
  Community1 <- V(g)[fc$membership==1]$name
  Community2 <- V(g)[fc$membership==2]$name
  Community3 <- V(g)[fc$membership==3]$name
  Community4 <- V(g)[fc$membership==4]$name
  Community5 <- V(g)[fc$membership==5]$name
  Community6 <- V(g)[fc$membership==6]$name
  Community7 <- V(g)[fc$membership==7]$name
  Community8 <- V(g)[fc$membership==8]$name
  Community9 <- V(g)[fc$membership==9]$name
  Community10 <- V(g)[fc$membership==10]$name
  Community11 <- V(g)[fc$membership==11]$name
  Community12 <- V(g)[fc$membership==12]$name
  Community13 <- V(g)[fc$membership==13]$name
  Community14 <- V(g)[fc$membership==14]$name
  Community15 <- V(g)[fc$membership==15]$name
  Community16 <- V(g)[fc$membership==16]$name
  Community17 <- V(g)[fc$membership==17]$name
  Community18 <- V(g)[fc$membership==18]$name
  Community19 <- V(g)[fc$membership==19]$name
  Community20 <- V(g)[fc$membership==20]$name
  Community21 <- V(g)[fc$membership==21]$name
  Community22 <- V(g)[fc$membership==22]$name
  Community23 <- V(g)[fc$membership==23]$name
  Community24 <- V(g)[fc$membership==24]$name
  Community25 <- V(g)[fc$membership==25]$name
  Community26 <- V(g)[fc$membership==26]$name
  Community27 <- V(g)[fc$membership==27]$name
  Community28 <- V(g)[fc$membership==28]$name
  Community29 <- V(g)[fc$membership==29]$name
  Community30 <- V(g)[fc$membership==30]$name
  Community31 <- V(g)[fc$membership==31]$name
  Community32 <- V(g)[fc$membership==32]$name
  Community33 <- V(g)[fc$membership==33]$name
  Community34 <- V(g)[fc$membership==34]$name
  Community35 <- V(g)[fc$membership==35]$name
  Community36 <- V(g)[fc$membership==36]$name
  Community37 <- V(g)[fc$membership==37]$name
  Community38 <- V(g)[fc$membership==38]$name
  Community39 <- V(g)[fc$membership==39]$name
  Community40 <- V(g)[fc$membership==40]$name
  Community41 <- V(g)[fc$membership==41]$name
  Community42 <- V(g)[fc$membership==42]$name
  Community43 <- V(g)[fc$membership==43]$name
  Community44 <- V(g)[fc$membership==44]$name
  Community45 <- V(g)[fc$membership==45]$name
  Community46 <- V(g)[fc$membership==46]$name
  Community47 <- V(g)[fc$membership==47]$name
  Community48 <- V(g)[fc$membership==48]$name
  Community49 <- V(g)[fc$membership==49]$name
  Community50 <- V(g)[fc$membership==50]$name
  
  mod <- modularity(fc)
  subg1<-induced.subgraph(g, which(membership(fc)==1))
  dens1 <- ecount(subg1) / vcount(subg1)
  subg2<-induced.subgraph(g, which(membership(fc)==2))
  dens2 <- ecount(subg2) / vcount(subg2)
  subg3<-induced.subgraph(g, which(membership(fc)==3))
  dens3 <- ecount(subg3) / vcount(subg3)
  subg4<-induced.subgraph(g, which(membership(fc)==4))
  dens4 <- ecount(subg4) / vcount(subg4)
  subg5<-induced.subgraph(g, which(membership(fc)==5))
  dens5 <- ecount(subg5) / vcount(subg5)
  subg6<-induced.subgraph(g, which(membership(fc)==6))
  dens6 <- ecount(subg6) / vcount(subg6)
  subg7<-induced.subgraph(g, which(membership(fc)==7))
  dens7 <- ecount(subg7) / vcount(subg7)
  subg8<-induced.subgraph(g, which(membership(fc)==8))
  dens8 <- ecount(subg8) / vcount(subg8)
  subg9<-induced.subgraph(g, which(membership(fc)==9))
  dens9 <- ecount(subg9) / vcount(subg9)
  subg10<-induced.subgraph(g, which(membership(fc)==10))
  dens10 <- ecount(subg10) / vcount(subg10)
  subg11<-induced.subgraph(g, which(membership(fc)==11))
  dens11 <- ecount(subg11) / vcount(subg11)
  subg12<-induced.subgraph(g, which(membership(fc)==12))
  dens12 <- ecount(subg12) / vcount(subg12)
  subg13<-induced.subgraph(g, which(membership(fc)==13))
  dens13 <- ecount(subg13) / vcount(subg13)
  subg14<-induced.subgraph(g, which(membership(fc)==14))
  dens14 <- ecount(subg14) / vcount(subg14)
  subg15<-induced.subgraph(g, which(membership(fc)==15))
  dens15 <- ecount(subg15) / vcount(subg15)
  subg16<-induced.subgraph(g, which(membership(fc)==16))
  dens16 <- ecount(subg16) / vcount(subg16)
  subg17<-induced.subgraph(g, which(membership(fc)==17))
  dens17 <- ecount(subg17) / vcount(subg17)
  subg18<-induced.subgraph(g, which(membership(fc)==18))
  dens18 <- ecount(subg18) / vcount(subg18)
  subg19<-induced.subgraph(g, which(membership(fc)==19))
  dens19 <- ecount(subg19) / vcount(subg19)
  length(dens1)<-length(dens2)<-length(dens3)<-length(dens4)<-length(dens5)<-length(dens6)<-length(dens7)<-length(dens8)<-length(dens9)<-length(dens10)<-length(dens11)<-length(dens12)<-length(dens13)<-length(dens14)<-length(dens15)<-length(dens16)<-length(dens17)<-length(dens18)<-length(dens19)<-length(mod)<-length(Community1)<-length(Community2)<-length(Community3)<-length(Community4)<-length(Community5)<-length(Community6)<-length(Community7)<-length(Community8)<-length(Community9)<-length(Community10)<-length(Community11)<-length(Community12)<-length(Community13)<-length(Community14)<-length(Community15)<-length(Community16)<-length(Community17)<-length(Community18)<-length(Community19)<-length(Community20)<-length(Community21)<-length(Community22)<-length(Community23)<-length(Community24)<-length(Community25)<-length(Community26)<-length(Community27)<-length(Community28)<-length(Community29)<-length(Community30)<-length(Community31)<-length(Community32)<-length(Community33)<-length(Community34)<-length(Community35)<-length(Community36)<-length(Community37)<-length(Community38)<-length(Community39)<-length(Community40)<-length(Community41)<-length(Community42)<-length(Community43)<-length(Community44)<-length(Community45)<-length(Community46)<-length(Community47)<-length(Community48)<-length(Community49)<-length(Community50) <- max(c(length(Community1),length(Community2),length(Community3),length(Community4),length(Community5),length(Community6),length(Community7),length(Community8),length(Community9),length(Community10),length(Community11),length(Community12),length(Community13),length(Community14),length(Community15),length(Community16),length(Community17),length(Community18),length(Community19),length(Community20),length(Community21),length(Community22),length(Community23),length(Community24),length(Community25),length(Community26),length(Community27),length(Community28),length(Community29),length(Community30),length(Community31),length(Community32),length(Community33),length(Community34),length(Community35),length(Community36),length(Community37),length(Community38),length(Community39),length(Community40),length(Community41),length(Community42),length(Community43),length(Community44),length(Community45),length(Community46),length(Community47),length(Community48),length(Community49),length(Community50)))
  df<- as.data.frame(rbind(Community1,Community2,Community3,Community4,Community5,Community6,Community7,Community8,Community9,Community10,Community11,Community12,Community13,Community14,Community15,Community16,Community17,Community18,Community19,Community20,Community21,Community22,Community23,Community24,Community25,Community26,Community27,Community28,Community29,Community30,Community31,Community32,Community33,Community34,Community35,Community36,Community37,Community38,Community39,Community40,Community41,Community42,Community43,Community44,Community45,Community46,Community47,Community48,Community49,Community50))
  return (df);
}";
            File.AppendAllText(scriptFile, footer);
            Console.WriteLine("Script file ready.");
        }
    }
}
