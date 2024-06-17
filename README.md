
# Table of Contents

1.  [dev.metalisp.survey](#org0b55d6d)
    1.  [Introduction](#org44f7f75)
    2.  [Dependencies](#orgb8893f3)
    3.  [Mailing list](#org1e94521)
    4.  [Issue tracker](#org21705ad)
    5.  [News Feed](#org1538d08)
    6.  [Installation instructions](#orgbe27a7f)
        1.  [1. Install a Common Lisp implementation](#orgb1cd926)
        2.  [2. Set up ASDF](#org096f7ea)
        3.  [3. Organize the project directory](#org7e09293)
        4.  [4. Configure ASDF to find the project](#orge70699b)
        5.  [5. Load the project](#org3a2e8bb)
        6.  [6. Run the project](#orgaca1c5b)
        7.  [Optional: Example Initialization in .sbclrc](#org7f6f9be)
    7.  [License](#orgcd477b2)



<a id="org0b55d6d"></a>

# dev.metalisp.survey


<a id="org44f7f75"></a>

## Introduction

I am developing a simple web application in Common Lisp that facilitates the
execution of the System Usability Scale (SUS) questionnaire. This app allows
users to easily conduct usability evaluations by presenting the standard SUS
questions and collecting responses. The goal is to streamline the process of
gathering and analyzing usability feedback.

The survey software developed aims to simplify the creation, administration and
integration of questionnaires into existing websites or software systems. The
use of flexible templates should enable a high degree of reusability and
adaptability of the survey components. Users can easily combine and analyze
data from multiple questionnaires within a single study, leading to a deeper
understanding of the collected data. The software is self-hosted, which means
that the data is stored on its own servers. This approach reinforces data
protection and gives users complete control over their data, which is
particularly important for data-sensitive areas. Through these features, the
survey web application strives to make a valuable contribution to research
projects, market research and other application areas where accurate data
collection and analysis is crucial.


<a id="orgb8893f3"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="org1e94521"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org21705ad"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org1538d08"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="orgbe27a7f"></a>

## Installation instructions


<a id="orgb1cd926"></a>

### 1. Install a Common Lisp implementation

-   Ensure you have a Common Lisp implementation installed. Common options
    include SBCL (Steel Bank Common Lisp) and CCL (Clozure Common Lisp). You
    can download and install them from their respective websites:
    -   [SBCL](http://www.sbcl.org/)
    -   [CCL](https://ccl.clozure.com/)


<a id="org096f7ea"></a>

### 2. Set up ASDF

-   ASDF is typically bundled with modern Lisp implementations. However, if
    it&rsquo;s not present, you can download it from [ASDF&rsquo;s repository](https://gitlab.common-lisp.net/asdf/asdf).


<a id="org7e09293"></a>

### 3. Organize the project directory

-   Place the `dev.metalisp.survey` project in the `~/common-lisp`
    directory. Ensure the directory structure looks like this:
    
        ~/common-lisp/
          └── dev.metalisp.survey/
              ├── dev.metalisp.survey.asd
              └── src/
                  └── app.lisp


<a id="orge70699b"></a>

### 4. Configure ASDF to find the project

-   Open your Common Lisp REPL and run the following commands to set up the
    ASDF central registry:
    
        ;; Ensure ASDF is loaded
        (require :asdf)
        
        ;; Add ~/common-lisp to the ASDF central registry
        (push #p"~/common-lisp/" asdf:*central-registry*)


<a id="org3a2e8bb"></a>

### 5. Load the project

-   In your REPL, load the project by running:
    
        (asdf:load-system :dev.metalisp.survey)


<a id="orgaca1c5b"></a>

### 6. Run the project

-   After loading the system, you can run the main function or entry point of
    the project.
    `ml-survey:start`, you would execute:
    
        (ml-survey:start)


<a id="org7f6f9be"></a>

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


<a id="orgcd477b2"></a>

## License

MIT

