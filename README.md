
# Table of Contents

1.  [dev.metalisp.survey](#org1975482)
    1.  [Introduction](#org6e3b1a7)
    2.  [Design Goals](#org1d291c6)
    3.  [Dependencies](#org3ab2392)
    4.  [Mailing list](#org07ed01d)
    5.  [Issue tracker](#org60e4fbd)
    6.  [News Feed](#orgf18f06e)
    7.  [Installation instructions](#org82b39a2)
        1.  [With using Quicklisp](#org9e7f013)
    8.  [License](#orga10bc56)



<a id="org1975482"></a>

# dev.metalisp.survey


<a id="org6e3b1a7"></a>

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


<a id="org1d291c6"></a>

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


<a id="org3ab2392"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="org07ed01d"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org60e4fbd"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="orgf18f06e"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="org82b39a2"></a>

## Installation instructions


<a id="org9e7f013"></a>

### With using Quicklisp

Quicklisp is a highly recommended library manager for Common Lisp capable of
streamlining the process of installing and maintaining libraries. It simplifies
downloading, building, and loading libraries with a minimal fuss and supports
command line interaction.

This guide will demonstrate how to install Quicklisp on both Linux and Windows
operating systems.

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

4.  Load dev.metalisp.survey

    1.  Clone this repository and dev.metalisp.sbt
        
            git clone git@git.sr.ht:~marcuskammer/dev.metalisp.sbt ~/quicklisp/local-projects/
            git clone git@git.sr.ht:~marcuskammer/dev.metalisp.survey ~/quicklisp/local-projects/
    
    2.  Start sbcl and load dev.metalisp.survey
        
            (ql:quickload :dev.metalisp.survey)


<a id="orga10bc56"></a>

## License

MIT

