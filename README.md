
# Table of Contents

1.  [dev.metalisp.survey](#org69a1733)
    1.  [Introduction](#org2a7150a)
    2.  [Dependencies](#org1ee57c9)
    3.  [Mailing list](#org3357d0b)
    4.  [Issue tracker](#orgc414d01)
    5.  [News Feed](#org4946fdf)
    6.  [Installation instructions](#org9c4c36e)
        1.  [1. Install a Common Lisp implementation](#org3df3cc5)
        2.  [2. Set up ASDF](#org244fc7d)
        3.  [3. Organize your project directory](#org68b8a26)
        4.  [4. Configure ASDF to find your project](#org238e518)
        5.  [5. Load your project](#org3ada275)
        6.  [6. Run your project](#org72e0615)
        7.  [Optional: Example Initialization in .sbclrc](#orgc2db2d7)
    7.  [License](#org0e7424a)



<a id="org69a1733"></a>

# dev.metalisp.survey


<a id="org2a7150a"></a>

## Introduction

I am developing a simple web application in Common Lisp that facilitates the
execution of the System Usability Scale (SUS) questionnaire. This app allows
users to easily conduct usability evaluations by presenting the standard SUS
questions and collecting responses. The goal is to streamline the process of
gathering and analyzing usability feedback.


<a id="org1ee57c9"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="org3357d0b"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="orgc414d01"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org4946fdf"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="org9c4c36e"></a>

## Installation instructions


<a id="org3df3cc5"></a>

### 1. Install a Common Lisp implementation

-   Ensure you have a Common Lisp implementation installed. Common options
    include SBCL (Steel Bank Common Lisp) and CCL (Clozure Common Lisp). You
    can download and install them from their respective websites:
    -   [SBCL](http://www.sbcl.org/)
    -   [CCL](https://ccl.clozure.com/)


<a id="org244fc7d"></a>

### 2. Set up ASDF

-   ASDF is typically bundled with modern Lisp implementations. However, if
    it&rsquo;s not present, you can download it from [ASDF&rsquo;s repository](https://gitlab.common-lisp.net/asdf/asdf).


<a id="org68b8a26"></a>

### 3. Organize your project directory

-   Place the `dev.metalisp.survey` project in the `~/common-lisp`
    directory. Ensure the directory structure looks like this:
    
        ~/common-lisp/
          └── dev.metalisp.survey/
              ├── dev.metalisp.survey.asd
              └── src/
                  └── main.lisp


<a id="org238e518"></a>

### 4. Configure ASDF to find your project

-   Open your Common Lisp REPL and run the following commands to set up the
    ASDF central registry:
    
        ;; Ensure ASDF is loaded
        (require :asdf)
        
        ;; Add ~/common-lisp to the ASDF central registry
        (push #p"~/common-lisp/" asdf:*central-registry*)


<a id="org3ada275"></a>

### 5. Load your project

-   In your REPL, load your project by running:
    
        (asdf:load-system :dev.metalisp.survey)


<a id="org72e0615"></a>

### 6. Run your project

-   After loading the system, you can run the main function or entry point of
    your project. For example, if your main function is
    `dev.metalisp.survey:start`, you would execute:
    
        (dev.metalisp.survey:start)


<a id="orgc2db2d7"></a>

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


<a id="org0e7424a"></a>

## License

MIT

