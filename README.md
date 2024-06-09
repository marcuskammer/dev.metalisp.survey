
# Table of Contents

1.  [dev.metalisp.survey](#orgf738832)
    1.  [Introduction](#org47dadf1)
    2.  [Dependencies](#org4d5be1c)
    3.  [Mailing list](#orgccf3013)
    4.  [Issue tracker](#org7fa29d6)
    5.  [News Feed](#org2fa1e6f)
    6.  [Installation instructions](#org095797a)
        1.  [1. Install a Common Lisp implementation](#org3954d41)
        2.  [2. Set up ASDF](#org3468e36)
        3.  [3. Organize your project directory](#org2529f8d)
        4.  [4. Configure ASDF to find your project](#org026723c)
        5.  [5. Load your project](#orgcd4f33a)
        6.  [6. Run your project](#org3a3b3db)
        7.  [Optional: Example Initialization in .sbclrc](#orgaa8ab9c)
    7.  [License](#org156e0f4)



<a id="orgf738832"></a>

# dev.metalisp.survey


<a id="org47dadf1"></a>

## Introduction

I am developing a simple web application in Common Lisp that facilitates the
execution of the System Usability Scale (SUS) questionnaire. This app allows
users to easily conduct usability evaluations by presenting the standard SUS
questions and collecting responses. The goal is to streamline the process of
gathering and analyzing usability feedback.


<a id="org4d5be1c"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="orgccf3013"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org7fa29d6"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org2fa1e6f"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="org095797a"></a>

## Installation instructions


<a id="org3954d41"></a>

### 1. Install a Common Lisp implementation

-   Ensure you have a Common Lisp implementation installed. Common options
    include SBCL (Steel Bank Common Lisp) and CCL (Clozure Common Lisp). You
    can download and install them from their respective websites:
    -   [SBCL](http://www.sbcl.org/)
    -   [CCL](https://ccl.clozure.com/)


<a id="org3468e36"></a>

### 2. Set up ASDF

-   ASDF is typically bundled with modern Lisp implementations. However, if
    it&rsquo;s not present, you can download it from [ASDF&rsquo;s repository](https://gitlab.common-lisp.net/asdf/asdf).


<a id="org2529f8d"></a>

### 3. Organize your project directory

-   Place the `dev.metalisp.survey` project in the `~/common-lisp`
    directory. Ensure the directory structure looks like this:
    
        ~/common-lisp/
          └── dev.metalisp.survey/
              ├── dev.metalisp.survey.asd
              └── src/
                  └── app.lisp


<a id="org026723c"></a>

### 4. Configure ASDF to find your project

-   Open your Common Lisp REPL and run the following commands to set up the
    ASDF central registry:
    
        ;; Ensure ASDF is loaded
        (require :asdf)
        
        ;; Add ~/common-lisp to the ASDF central registry
        (push #p"~/common-lisp/" asdf:*central-registry*)


<a id="orgcd4f33a"></a>

### 5. Load your project

-   In your REPL, load the project by running:
    
        (asdf:load-system :dev.metalisp.survey)


<a id="org3a3b3db"></a>

### 6. Run your project

-   After loading the system, you can run the main function or entry point of
    the project.
    `dev.metalisp.survey:start`, you would execute:
    
        (dev.metalisp.survey:start)


<a id="orgaa8ab9c"></a>

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


<a id="org156e0f4"></a>

## License

MIT

