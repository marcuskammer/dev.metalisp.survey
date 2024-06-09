
# Table of Contents

1.  [dev.metalisp.survey](#orgfc20458)
    1.  [Introduction](#org82e49d0)
    2.  [Dependencies](#org0384d14)
    3.  [Mailing list](#orgcddacc5)
    4.  [Issue tracker](#org6369bf7)
    5.  [News Feed](#orgfbb79a0)
    6.  [Installation instructions](#org9b4ca7a)
        1.  [1. Install a Common Lisp implementation](#org8cdd8c0)
        2.  [2. Set up ASDF](#orgf77351d)
        3.  [3. Organize the project directory](#org6c13c6a)
        4.  [4. Configure ASDF to find the project](#org1a1ebc5)
        5.  [5. Load the project](#orgacb4ed1)
        6.  [6. Run the project](#org3908f2a)
        7.  [Optional: Example Initialization in .sbclrc](#org6a80305)
    7.  [License](#org4d30f3e)



<a id="orgfc20458"></a>

# dev.metalisp.survey


<a id="org82e49d0"></a>

## Introduction

I am developing a simple web application in Common Lisp that facilitates the
execution of the System Usability Scale (SUS) questionnaire. This app allows
users to easily conduct usability evaluations by presenting the standard SUS
questions and collecting responses. The goal is to streamline the process of
gathering and analyzing usability feedback.


<a id="org0384d14"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="orgcddacc5"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org6369bf7"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="orgfbb79a0"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="org9b4ca7a"></a>

## Installation instructions


<a id="org8cdd8c0"></a>

### 1. Install a Common Lisp implementation

-   Ensure you have a Common Lisp implementation installed. Common options
    include SBCL (Steel Bank Common Lisp) and CCL (Clozure Common Lisp). You
    can download and install them from their respective websites:
    -   [SBCL](http://www.sbcl.org/)
    -   [CCL](https://ccl.clozure.com/)


<a id="orgf77351d"></a>

### 2. Set up ASDF

-   ASDF is typically bundled with modern Lisp implementations. However, if
    it&rsquo;s not present, you can download it from [ASDF&rsquo;s repository](https://gitlab.common-lisp.net/asdf/asdf).


<a id="org6c13c6a"></a>

### 3. Organize the project directory

-   Place the `dev.metalisp.survey` project in the `~/common-lisp`
    directory. Ensure the directory structure looks like this:
    
        ~/common-lisp/
          └── dev.metalisp.survey/
              ├── dev.metalisp.survey.asd
              └── src/
                  └── app.lisp


<a id="org1a1ebc5"></a>

### 4. Configure ASDF to find the project

-   Open your Common Lisp REPL and run the following commands to set up the
    ASDF central registry:
    
        ;; Ensure ASDF is loaded
        (require :asdf)
        
        ;; Add ~/common-lisp to the ASDF central registry
        (push #p"~/common-lisp/" asdf:*central-registry*)


<a id="orgacb4ed1"></a>

### 5. Load the project

-   In your REPL, load the project by running:
    
        (asdf:load-system :dev.metalisp.survey)


<a id="org3908f2a"></a>

### 6. Run the project

-   After loading the system, you can run the main function or entry point of
    the project.
    `ml-survey:start`, you would execute:
    
        (ml-survey:start)


<a id="org6a80305"></a>

### Optional: Example Initialization in .sbclrc

To make the ASDF configuration persistent across REPL sessions, you can add the
setup to your `.sbclrc` file:

1.  Edit `.sbclrc`
    -   Open (or create) the `.sbclrc` file in your home directory and add the
        following lines:
        
            (require :asdf)
            (push #p"~/common-lisp/" asdf:*central-registry*)

2.  Reload SBCL
    -   The next time you start SBCL, it will automatically include the
        `~/common-lisp` directory in the ASDF central registry.


<a id="org4d30f3e"></a>

## License

MIT

