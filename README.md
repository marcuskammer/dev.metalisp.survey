
# Table of Contents

1.  [dev.metalisp.survey](#orgd64d9bb)
    1.  [Introduction](#org955868c)
        1.  [Design Goals](#org71faa7e)
    2.  [Dependencies](#orgfa4e430)
    3.  [Mailing list](#org1d8307c)
    4.  [Issue tracker](#org406575f)
    5.  [News Feed](#orgd8c6a45)
    6.  [Installation instructions](#orga5c9fc3)
        1.  [1. Install a Common Lisp implementation](#org67de338)
        2.  [2. Set up ASDF](#org762005d)
        3.  [3. Organize the project directory](#orga1725f5)
        4.  [4. Configure ASDF to find the project](#org5e3ce31)
        5.  [5. Load the project](#org9f571d3)
        6.  [6. Run the project](#org6538bb4)
        7.  [Optional: Example Initialization in .sbclrc](#org01cbab2)
    7.  [License](#org2b2279f)



<a id="orgd64d9bb"></a>

# dev.metalisp.survey


<a id="org955868c"></a>

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


<a id="org71faa7e"></a>

### Design Goals

-   Integrate questionnaires into your own website or software: The software
    enables seamless integration of questionnaires into existing digital
    environments to improve user experience and simplify data collection.

-   Reusability through templates: Through customizable templates, the software
    offers an efficient solution for creating repeatable and consistent survey
    formats for various use cases.

-   Ability to combine data from multiple questionnaires into one study: Users
    can merge and aggregate data from different questionnaires to provide more
    comprehensive insights and analysis in a single study.

-   Data protection through self-hosting: The self-hosting option ensures
    complete data control and strengthens data protection as sensitive
    information does not need to be transferred to external servers.


<a id="orgfa4e430"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="org1d8307c"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org406575f"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="orgd8c6a45"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="orga5c9fc3"></a>

## Installation instructions


<a id="org67de338"></a>

### 1. Install a Common Lisp implementation

-   Ensure you have a Common Lisp implementation installed. Common options
    include SBCL (Steel Bank Common Lisp) and CCL (Clozure Common Lisp). You
    can download and install them from their respective websites:
    -   [SBCL](http://www.sbcl.org/)
    -   [CCL](https://ccl.clozure.com/)


<a id="org762005d"></a>

### 2. Set up ASDF

-   ASDF is typically bundled with modern Lisp implementations. However, if
    it&rsquo;s not present, you can download it from [ASDF&rsquo;s repository](https://gitlab.common-lisp.net/asdf/asdf).


<a id="orga1725f5"></a>

### 3. Organize the project directory

-   Place the `dev.metalisp.survey` project in the `~/common-lisp`
    directory. Ensure the directory structure looks like this:
    
        ~/common-lisp/
          └── dev.metalisp.survey/
              ├── dev.metalisp.survey.asd
              └── src/
                  └── app.lisp


<a id="org5e3ce31"></a>

### 4. Configure ASDF to find the project

-   Open your Common Lisp REPL and run the following commands to set up the
    ASDF central registry:
    
        ;; Ensure ASDF is loaded
        (require :asdf)
        
        ;; Add ~/common-lisp to the ASDF central registry
        (push #p"~/common-lisp/" asdf:*central-registry*)


<a id="org9f571d3"></a>

### 5. Load the project

-   In your REPL, load the project by running:
    
        (asdf:load-system :dev.metalisp.survey)


<a id="org6538bb4"></a>

### 6. Run the project

-   After loading the system, you can run the main function or entry point of
    the project.
    `ml-survey:start`, you would execute:
    
        (ml-survey:start)


<a id="org01cbab2"></a>

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


<a id="org2b2279f"></a>

## License

MIT

