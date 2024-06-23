
# Table of Contents

1.  [dev.metalisp.survey](#orgc66cfcc)
    1.  [Introduction](#org3352779)
    2.  [Design Goals](#org40a77cb)
    3.  [Dependencies](#org6304ba7)
    4.  [Mailing list](#org965470f)
    5.  [Issue tracker](#org00db5dc)
    6.  [News Feed](#org4d6d6d9)
    7.  [Installation instructions](#org7eb180d)
        1.  [Without using Quicklisp](#org926b3bc)
        2.  [With using Quicklisp](#org07715ac)
    8.  [License](#orgef05e23)



<a id="orgc66cfcc"></a>

# dev.metalisp.survey


<a id="org3352779"></a>

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


<a id="org40a77cb"></a>

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


<a id="org6304ba7"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="org965470f"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org00db5dc"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org4d6d6d9"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="org7eb180d"></a>

## Installation instructions


<a id="org926b3bc"></a>

### Without using Quicklisp

1.  1. Install a Common Lisp implementation

    -   Ensure you have a Common Lisp implementation installed. Common options
        include SBCL (Steel Bank Common Lisp) and CCL (Clozure Common Lisp). You
        can download and install them from their respective websites:
        -   [SBCL](http://www.sbcl.org/)
        -   [CCL](https://ccl.clozure.com/)

2.  2. Set up ASDF

    -   ASDF is typically bundled with modern Lisp implementations. However, if
        it&rsquo;s not present, you can download it from [ASDF&rsquo;s repository](https://gitlab.common-lisp.net/asdf/asdf).

3.  3. Organize the project directory

    -   Place the `dev.metalisp.survey` project in the `~/common-lisp`
        directory. Ensure the directory structure looks like this:
        
            ~/common-lisp/
              └── dev.metalisp.survey/
                  ├── dev.metalisp.survey.asd
                  └── src/
                      └── app.lisp

4.  4. Configure ASDF to find the project

    -   Open your Common Lisp REPL and run the following commands to set up the
        ASDF central registry:
        
            ;; Ensure ASDF is loaded
            (require :asdf)
            
            ;; Add ~/common-lisp to the ASDF central registry
            (push #p"~/common-lisp/" asdf:*central-registry*)

5.  5. Load the project

    -   In your REPL, load the project by running:
        
            (asdf:load-system :dev.metalisp.survey)

6.  6. Run the project

    -   After loading the system, you can run the main function or entry point of
        the project.
        `ml-survey:start`, you would execute:
        
            (ml-survey:start)

7.  Optional: Example Initialization in .sbclrc

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


<a id="org07715ac"></a>

### With using Quicklisp

Quicklisp is a highly recommended library manager for Common Lisp capable of
streamlining the process of installing and maintaining libraries. It simplifies
downloading, building, and loading libraries with a minimal fuss and supports
command line interaction.

This guide will demonstrate how to install Quicklisp on both Linux and Windows operating systems.

1.  Why Use Quicklisp?

    Quicklisp offers several advantages for Common Lisp development:
    
    -   **Ease of Use:** It simplifies the installation process of common Lisp
        libraries, handling dependencies automatically.
    -   **Extensive Library Collection:** Quicklisp connects to a vast repository of
        libraries, making it easy to find and install almost any library you need for
        a project.
    -   **Regular Updates:** Quicklisp updates its library list monthly, so you always
        have access to the latest versions.
    -   **Integration:** It integrates well with many Lisp environments and tools,
        enhancing your development workflow.

2.  On Linux

    1.  **Install a Common Lisp implementation:**
        For example, to install SBCL:
        
            sudo apt-get install sbcl
    
    2.  **Download Quicklisp Installer:**
        Open a terminal and run:
        
            curl -O https://beta.quicklisp.org/quicklisp.lisp
    
    3.  **Install Quicklisp:**
        With Lisp implementation installed (e.g., SBCL), run:
        
            sbcl --load quicklisp.lisp
        
        Within the Lisp environment, enter:
        
            (quicklisp-quickstart:install)
    
    4.  **Integrate Quicklisp with your Lisp environment:**
        To automatically load Quicklisp on Lisp startup:
        
            (ql:add-to-init-file)
        
        Follow the on-screen instructions, then exit Lisp:
        
            (quit)

3.  On Windows

    1.  **Install a Common Lisp implementation:**
        Download and install, for example, SBCL from <http://www.sbcl.org/platform-table.html>
    
    2.  **Download Quicklisp Installer:**
        Open PowerShell and run:
        
            Invoke-WebRequest -Uri https://beta.quicklisp.org/quicklisp.lisp -OutFile quicklisp.lisp
    
    3.  **Install Quicklisp:**
        Open installed Lisp (e.g., SBCL) shell by searching it in the start menu. Then run:
        
            --load quicklisp.lisp
        
        Within the Lisp REPL, execute:
        
            (quicklisp-quickstart:install)
    
    4.  **Set Up Quicklisp:**
        To enable Quicklisp every time Lisp starts:
        
            (ql:add-to-init-file)
        
        Follow the steps provided, then exit:
        
            (quit)


<a id="orgef05e23"></a>

## License

MIT

