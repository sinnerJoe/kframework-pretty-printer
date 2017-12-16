# Pretty printer for krun and HTML generator for K files

## Install

First of all [install KFramework](https://profs.info.uaic.ro/~arusoaie.andrei/lectures/PLP/2017/week1/lab1.html)
, then you have 2 options:
* Download the apps from bin/<your_os> folder and place them in a folder from PATH variable (place them in kframework folder along with regular krun executable). **Note:** In case you can't execute binaries under linux because you don't have permissions to do so use ```chmod +x pkrun && chmod +x pkompile```.

* Build using stack tool: 
     1. [Install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
     2. ```git clone https://github.com/wildProgrammer/kframework-pretty-printer``` copy the repository
     3. ```cd kframework-pretty-printer``` go into downloaded folder
     4. ```stack setup```
     5. ```stack build``` 
     6. Place the generated executables from .stack-work folder (example: ```.stack-work/install/x86_64-linux-nopie/lts-9.13/8.0.2/bin```) into a folder from PATH variable.

 Instead of steps **v** and **vi** you can use ```stack install``` command which builds executables and copies them in a place where programs are tipically installed(and accesible from PATH variable) in your system (for example in Arch Linux it is```/home/username/.local/bin/```, for Windows 10 ```C:\Users\%username%\AppData\Roaming\local\bin```)


## Usage

There are 2 programs:
1. pkompile - ```pkompile [--html] <k_file>.k```
Functionality: 
     * Generates colors.conf file inside ```*-kompiled``` folder, it contains colors found in the attributes of tags in K configuration.
     * By using *--html* flag the program generates a HTML file which vizualizes configuration tags as cells with labels and content.
2. pkrun - ```pkrun <arg>``` where arg is usually a *\*.k* file but it can be anything that is accepted by krun. The program isn't tested with other arguments than aforementioned single argument so use it at your own risk and frustration.
     * Parses the output of **krun** and transforms it in a more human-readable form by indenting tags and the contents within.

### Examples of output

**krun**
![krun](https://s33.postimg.org/w2igyk20v/krun.png)

**pkrun**
![pkrun](https://s33.postimg.org/xitzg4rwv/pkrun_default.png)

**pkrun + colors.conf**
![pkrun + colors.conf](https://s33.postimg.org/ws19axxfj/pkrun_coloured.png)

## Known Issues

- pkrun can't print coloured text in windows terminal. This can be solved by installing some POSIX compliant shell like cygwin or msys2.
- pkompile and pkrun assumes that you don't have other characters in you tag names than lowercase and uppercase latin letters, naming tags differently may break them
- pkompile won't recognize configuration if the opening of the first tag isn't one the same line with keyword ```configuration```
- you won't see intermediate outputs from your program if it's waiting for input but will still be able to input data. For example: 
```print("x="); input(x);``` - you won't see ```x=``` before you introduce x's value.
- kompile can't handle K comments in lines between tags, it won't necessarily generate errors, as a rule o thumb it is recommended to write them in other places.  
