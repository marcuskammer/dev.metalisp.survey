
# Table of Contents

1.  [dev.metalisp.survey](#orgb0084b6)
    1.  [Disclaimer](#orgf3d6bc4)
    2.  [Introduction](#orgec63d69)
    3.  [Design Goals](#orgc44dc9a)
    4.  [Features](#org588692e)
    5.  [Dependencies](#org754aa24)
    6.  [Mailing list](#orgbb08363)
    7.  [Issue tracker](#org5e74cac)
    8.  [News Feed](#orgdff4099)
    9.  [XMPP Chat](#org6d84f8a)
    10. [Installation instructions](#org66f6bfb)
        1.  [With using Quicklisp](#org416a5d4)
    11. [Screenshot](#orgac3d4c3)
    12. [License](#org5a15fc3)
        1.  [MIT](#org484072e)



<a id="orgb0084b6"></a>

# dev.metalisp.survey

Made with Love ❤️ and Common Lisp


<a id="orgf3d6bc4"></a>

## Disclaimer

Libre software (LS) is often misunderstood as entirely free, including support
and maintenance. While LS is freely available to use, modify, and distribute,
users bear the responsibility for its integration, upkeep, and
troubleshooting. Unlike commercial software, which typically includes support
services, LS relies on community or paid third-party support, if
available. This model empowers users with flexibility and control but requires
a commitment to managing the software effectively. Therefore, adopting LS
demands a proactive approach to handling any issues and ensuring the software
meets organizational needs.


<a id="orgec63d69"></a>

## Introduction

I am developing a web application using Common Lisp, specifically tailored for
efficiently administering the

-   [System Usability Scale](https://git.sr.ht/~marcuskammer/dev.metalisp.survey/tree/main/item/src/views/questionnaires/en/sus.lisp) ([wikipedia](https://en.wikipedia.org/wiki/System_usability_scale)),
-   User Experience Questionnaire ([website](https://www.ueq-online.org/)),
-   and Visual Aesthetics of Websites Inventory ([website](https://meinald.de/forschung/visawi/), [researchgate](https://www.researchgate.net/publication/274649948_VisAWI_Manual_Visual_Aesthetics_of_Websites_Inventory_and_the_short_form_VisAWI-S_Short_Visual_Aesthetics_of_Websites_Inventory))

to streamline usability evaluations. This application not only presents these
various questions and collects responses but also simplifies the entire process
of gathering and analyzing usability feedback. Designed to enhance the ease of
creating, managing, and integrating questionnaires within existing websites or
software systems, it leverages flexible templates to offer high levels of
reusability and adaptability. The software allows for the synthesis of data
from multiple questionnaires into a single study, enabling deeper insights. As
a self-hosted solution, it provides enhanced data protection, granting users
full control over their data—a crucial feature in environments handling
sensitive data. This makes it an invaluable tool for research projects, market
research, and other sectors that require precise data collection and analysis.


<a id="orgc44dc9a"></a>

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


<a id="org588692e"></a>

## Features

1.  **Questionnaires**: Questionnaires can be defined using a Domain-Specific
    Language (DSL) developed for dev.metalisp.sbt. The benefit of defining forms
    in files is that they can be versioned using GIT, providing better control
    over changes and updates.

2.  **Simplicity**: All data is saved to files, eliminating the need for a
    database. This reduces costs for self-hosting and simplifies the setup and
    maintenance of the application.


<a id="org754aa24"></a>

## Dependencies

-   <https://github.com/edicl/hunchentoot>
-   <https://git.sr.ht/~marcuskammer/dev.metalisp.sbt>


<a id="orgbb08363"></a>

## Mailing list

-   <https://lists.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="org5e74cac"></a>

## Issue tracker

-   <https://todo.sr.ht/~marcuskammer/dev.metalisp.survey>


<a id="orgdff4099"></a>

## News Feed

-   <https://git.sr.ht/~marcuskammer/dev.metalisp.survey/log/main/rss.xml>


<a id="org6d84f8a"></a>

## XMPP Chat

-   


<a id="org66f6bfb"></a>

## Installation instructions


<a id="org416a5d4"></a>

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


<a id="orgac3d4c3"></a>

## Screenshot

![img](https://git.sr.ht/~marcuskammer/dev.metalisp.survey/blob/main/screenshot.png)


<a id="org5a15fc3"></a>

## License


<a id="org484072e"></a>

### MIT

Copyright (c) 2024 Marcus Kammer

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
&ldquo;Software&rdquo;), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED &ldquo;AS IS&rdquo;, WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

