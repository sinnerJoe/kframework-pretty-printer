# Pretty printer for krun and HTML generator for K files

#Install
a. Download the apps from bin/<your_os> folder and place them in a folder from PATH variable (place them in kframework folder along with regular krun executable).
b. Use stack to compile:
Build using stack tool: 
0) [Install KFramework](https://github.com/andreiarusoaie/k)
1) [Install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2) 
-```git clone https://github.com/wildProgrammer/kframework-pretty-printer``` copy the repository
-```cd kframework-pretty-printer``` go into downloaded folder
- *```stack build``` and then place the generated executables which you can find in .stack-work folder (example: .stack-work/install/x86_64-linux-nopie/lts-9.13/8.0.2/bin) into a folder from PATH variable.
  **or**
  *```stack install``` - it builds executables and copies them in a place where programs are tipically installed in your system (for example in Arch Linux it is```/home/username/.local/bin/```)


#Usage
There are 2 programs:
1. pkompile - ```pkompile [--html] <k_file>.k```
Functionality: 
* Generates colors.conf file inside <lang>-kompiled folder, it contains colors found in the attributes of tags in K configuration.
* By using *--html* flag the program generates a HTML file which vizualizes configuration tags as cells with labels and content.
2. pkrun - ```pkrun <arg>``` where arg is usually a **.k* file but it can be anything that is accepted by krun. The program isn't tested with other arguments than aforementioned single argument so use it at your own risk and frustration.
* Parses the output of **krun** and transforms it in a more human-readable form by indenting tags and the contents within.
