%{ 
int flag = 0; 
/*Creating file pointer to write results to an output file*/
    FILE *fp_out;
%} 

/*Regular expressions according to g++ syntax*/
VALUE [1-9][0-9]*|"0"
IDENTIFIER  [a-zA-Z_][a-zA-Z0-9_]*
COMMENT ";;".*

%%
"true" { fprintf(fp_out,"KW_TRUE\n");}
"false" { fprintf(fp_out,"KW_FALSE\n");}
"and" { fprintf(fp_out,"KW_AND\n");}
"or" { fprintf(fp_out,"KW_OR\n");}
"not" { fprintf(fp_out,"KW_NOT\n");}
"equal" { fprintf(fp_out,"KW_EQUAL\n");} 
"for" { fprintf(fp_out,"KW_FOR\n");}
"if" { fprintf(fp_out,"KW_IF\n");}
"exit" { fprintf(fp_out,"KW_EXIT\n");}
"less" { fprintf(fp_out,"KW_LESS\n");}
"nil" { fprintf(fp_out,"KW_NIL\n");}
"list" { fprintf(fp_out,"KW_LIST\n");}
"append" { fprintf(fp_out,"KW_APPEND\n");}
"concat" { fprintf(fp_out,"KW_CONCAT\n");}
"set" { fprintf(fp_out,"KW_SET\n");}
"deffun" { fprintf(fp_out,"KW_DEFFUN\n");}
"load" { fprintf(fp_out,"KW_LOAD\n");}
"disp" { fprintf(fp_out,"KW_DISP\n");}
"+" { fprintf(fp_out,"OP_PLUS\n");}
"-" { fprintf(fp_out,"OP_MINUS\n");}
"/" { fprintf(fp_out,"OP_DIV\n");}
"*" { fprintf(fp_out,"OP_MULT\n");}
"(" { fprintf(fp_out,"OP_OP\n");}
")" { fprintf(fp_out,"OP_CP\n");} 
"**" { fprintf(fp_out,"OP_DBLMULT\n");}
"\"" {    
            if(flag==0){ // operator is OC
                fprintf(fp_out,"OP_OC\n");
                flag=1;
            }    

            else{  // operator is CC
                fprintf(fp_out,"OP_CC\n");
                flag=0;
            }  
        }

{COMMENT} {fprintf (fp_out,"COMMENT\n");}
"," { fprintf(fp_out,"OP_COMMA\n");}
{VALUE} { fprintf(fp_out,"VALUE\n");}
{IDENTIFIER} {fprintf(fp_out,"IDENTIFIER\n");}
"\n"|"\t"|" " { }

. { fprintf (fp_out,"SYNTAX ERROR %s cannot be tokonized \n",yytext); } 
"\n\n" {return 0; }

%%

int yywrap(){} 

//main function
int main(int argc, char* argv[]){
   
    /* open the file for writing*/
    fp_out = fopen ("parsed_cpp.txt","w");
    
    /*if program starts with a filename reads file
     and interpreter does the lexical analysis from the file.*/
    if(argc>1){ 

        FILE *filepointer;
        filepointer = fopen(argv[1],"r");  
        yyin = filepointer; //reads file and does lexical analysis
        yylex(); 
        fclose(filepointer);

    }

    else{

        yyin = stdin; //interpreter is doing lexical analysis.
        yylex();
            
    }
    fclose(fp_out);

    return 0;
}//end of main