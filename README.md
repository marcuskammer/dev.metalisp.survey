
# Table of Contents

1.  [dev.metalisp.survey](#org2897deb)
    1.  [Introduction](#orgfb6d5f7)
    2.  [Design Goals](#orgdfc28ab)
    3.  [Dependencies](#org6b889ff)
    4.  [Mailing list](#orgf5055cc)
    5.  [Issue tracker](#orgc4a8b25)
    6.  [News Feed](#org2f18653)
    7.  [Installation instructions](#orgfd8430c)
        1.  [1. Install a Common Lisp implementation](#orgf30cb52)
        2.  [2. Set up ASDF](#org5676561)
        3.  [3. Organize the project directory](#org0f1ad40)
        4.  [4. Configure ASDF to find the project](#org316ff10)
        5.  [5. Load the project](#org0a0ce20)
        6.  [6. Run the project](#org201712e)
        7.  [Optional: Example Initialization in .sbclrc](#org1e6c49d)
    8.  [License](#org5cd04ac)



<a id="org2897deb"></a>

# dev.metalisp.survey


<a id="orgfb6d5f7"></a>

## Introduction

I am developing a web application using Common Lisp, designed to administer the
System Usability Scale (SUS) questionnaire efficiently. This application
simplifies the process of conducting usability evaluations by presenting SUS
questions and collecting responses, streamlining both the gathering and
analysis of usability feedback.

The software focuses on enhancing the ease of creating, managing, and
integrating questionnaires within existing websites or software. Leveraging
flexible templates, it offers a high level of reusability and
adaptability. Data from multiple questionnaires can be synthesized within a
single study, facilitating more profound insights. As a self-hosted solution,
it assures enhanced data protection, granting users complete control over their
data, a crucial feature for sensitive data environments. This application is
aimed at significantly contributing to research projects, market research, and
other fields where precise data collection and analysis are vital.


<a id="orgdfc28ab"></a>

## Design Goals

1.  **Integration**: The app integrates seamlessly into existing digital
    platforms, enhancing user experience and simplifying data capture.

2.  **Reusability and Adaptability**: Customizable templates allow for the
    creation of consistent and repeatable survey formats suitable for various
    applications.

3.  **Data Synthesis**: It supports combining data from multiple questionnaires
    into a single study, providing broader and more comprehensive analytical
    insights.

4.  **Privacy and Control**: With self-hosting, the software ensures complete data
    sovereignty and enhances privacy, avoiding the need to transfer sensitive
    data to external servers.

5.  **Accessibility**: The application is designed following the Web
    Content Accessibility Guidelines (WCAG) provided by WebAIM,
    ensuring that the survey is accessible to all users, including
    those with disabilities. This includes features such as keyboard
    navigation, screen reader compatibility, and high contrast modes to
    accommodate users with varying needs and abilities.


<a id="org6b889ff"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="orgf5055cc"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="orgc4a8b25"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org2f18653"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="orgfd8430c"></a>

## Installation instructions


<a id="orgf30cb52"></a>

### 1. Install a Common Lisp implementation

-   Ensure you have a Common Lisp implementation installed. Common options
    include SBCL (Steel Bank Common Lisp) and CCL (Clozure Common Lisp). You
    can download and install them from their respective websites:
    -   [SBCL](http://www.sbcl.org/)
    -   [CCL](https://ccl.clozure.com/)


<a id="org5676561"></a>

### 2. Set up ASDF

-   ASDF is typically bundled with modern Lisp implementations. However, if
    it&rsquo;s not present, you can download it from [ASDF&rsquo;s repository](https://gitlab.common-lisp.net/asdf/asdf).


<a id="org0f1ad40"></a>

### 3. Organize the project directory

-   Place the `dev.metalisp.survey` project in the `~/common-lisp`
    directory. Ensure the directory structure looks like this:
    
        ~/common-lisp/
          └── dev.metalisp.survey/
              ├── dev.metalisp.survey.asd
              └── src/
                  └── app.lisp


<a id="org316ff10"></a>

### 4. Configure ASDF to find the project

-   Open your Common Lisp REPL and run the following commands to set up the
    ASDF central registry:
    
        ;; Ensure ASDF is loaded
        (require :asdf)
        
        ;; Add ~/common-lisp to the ASDF central registry
        (push #p"~/common-lisp/" asdf:*central-registry*)


<a id="org0a0ce20"></a>

### 5. Load the project

-   In your REPL, load the project by running:
    
        (asdf:load-system :dev.metalisp.survey)


<a id="org201712e"></a>

### 6. Run the project

-   After loading the system, you can run the main function or entry point of
    the project.
    `ml-survey:start`, you would execute:
    
        (ml-survey:start)


<a id="org1e6c49d"></a>

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


<a id="org5cd04ac"></a>

## License

MIT

