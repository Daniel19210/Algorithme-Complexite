%rapport AlgoM1
%R. RAFFIN, UBourgogne, 01/02/2023
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{rapportAlgoM1}[2023/01/01 classe Latex rapport algoM1]

\LoadClass[12pt]{book}
\RequirePackage[paperheight=29.7cm, paperwidth=21cm,
%includehead,
nomarginpar,
textwidth=18cm,
textheight=23cm,
headheight=1.5cm,
lmargin=2cm,
rmargin=2cm,
%showframe,
]{geometry}

\RequirePackage{lastpage}

%\renewcommand{\section}{%
%	\@startsection
%	{section}{1}{0pt}{-1.5ex plus -1ex minus -.2ex}%
%	{1ex plus .2ex}{\large\sffamily\slshape\headlinecolor}%
%}
%

\newcommand{\headlinecolor}{\normalcolor}
\RequirePackage{xcolor}

\RequirePackage{tikz}

\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage{framed,eso-pic,graphicx,url}

\RequirePackage[french]{babel}


%headers et footers avec FancyHeader
\RequirePackage{fancyhdr}
\pagestyle{fancy}

\lhead{{\small \auteurA \ \auteurB \ \auteurC}}
\rhead{Projet <<\ Algorithmique et complexité\ >>}
\chead{}
\lfoot{{\small \textit{\sujet}}}
\rfoot{\textsf{\small{\thepage}}}%/\pageref{LastPage}}}}
\cfoot{}

%%"boîte" colorée pour le sujet de la page de garde
\RequirePackage{tcolorbox}

%pour les matrices et les notations mathématiques
\RequirePackage{amsmath}

%%URL et lien dasn le texte
\RequirePackage{hyperref}
\RequirePackage{url}

%%au besoin
\RequirePackage{verbatim}

%%pour les figures et subfigures
\RequirePackage{caption}
\RequirePackage{subcaption}

%%listings
\RequirePackage{listings}
\lstset{language=caml, breaklines=true,
	backgroundcolor=\color{pastelgray},
	fillcolor=\color{white},
	rulesepcolor=\color{black},
	rulecolor=\color{gray},
	columns=fullflexible,
	showstringspaces=false,          % underline spaces within strings only
	%framesep=14pt,
	%framerule=0pt,
	showspaces=false,                % show spaces everywhere adding particul
	numbers=left,                   % where to put the line-numbers; possible values are (none, left, right)
	%numbersep=5pt,                   % how far the line-numbers are from the code
	numberstyle=\tiny\color{black}, % the style that is used for the line-numbersframe=single,
	basicstyle=\footnotesize\ttfamily,
	frame=shadowbox,
	frameround=tttt,
	keywordstyle=\bfseries\color{olive},
	commentstyle=\itshape\color{purple},
	identifierstyle=\color{blue},
	stringstyle=\color{orange},
	literate=
	{á}{{\'a}}1 {é}{{\'e}}1 {í}{{\'i}}1 {ó}{{\'o}}1 {ú}{{\'u}}1
	{Á}{{\'A}}1 {É}{{\'E}}1 {Í}{{\'I}}1 {Ó}{{\'O}}1 {Ú}{{\'U}}1
	{à}{{\`a}}1 {è}{{\`e}}1 {ì}{{\`i}}1 {ò}{{\`o}}1 {ù}{{\`u}}1
	{À}{{\`A}}1 {È}{{\'E}}1 {Ì}{{\`I}}1 {Ò}{{\`O}}1 {Ù}{{\`U}}1
	{ä}{{\"a}}1 {ë}{{\"e}}1 {ï}{{\"i}}1 {ö}{{\"o}}1 {ü}{{\"u}}1
	{Ä}{{\"A}}1 {Ë}{{\"E}}1 {Ï}{{\"I}}1 {Ö}{{\"O}}1 {Ü}{{\"U}}1
	{â}{{\^a}}1 {ê}{{\^e}}1 {î}{{\^i}}1 {ô}{{\^o}}1 {û}{{\^u}}1
	{Â}{{\^A}}1 {Ê}{{\^E}}1 {Î}{{\^I}}1 {Ô}{{\^O}}1 {Û}{{\^U}}1
	{Ã}{{\~A}}1 {ã}{{\~a}}1 {Õ}{{\~O}}1 {õ}{{\~o}}1
	{œ}{{\oe}}1 {Œ}{{\OE}}1 {æ}{{\ae}}1 {Æ}{{\AE}}1 {ß}{{\ss}}1
	{ű}{{\H{u}}}1 {Ű}{{\H{U}}}1 {ő}{{\H{o}}}1 {Ő}{{\H{O}}}1
	{ç}{{\c c}}1 {Ç}{{\c C}}1 {ø}{{\o}}1 {å}{{\r a}}1 {Å}{{\r A}}1
	{€}{{\euro}}1 {£}{{\pounds}}1 {«}{{\guillemotleft}}1
	{»}{{\guillemotright}}1 {ñ}{{\~n}}1 {Ñ}{{\~N}}1 {¿}{{?`}}1
}



\setcounter{secnumdepth}{2}
\setcounter{chapter}{1}

% % % % % % % % % % % % % % % %
% http://latexcolor.com/
% % % % % % % % % % % % % % % %
\definecolor{bleudefrance}{rgb}{0.19, 0.55, 0.91} %Bleu de France #318CE7
\definecolor{pastelgray}{rgb}{0.93, 0.93, 0.93}
% % % % % % % % % % % % % % % %
% http://ethanschoonover.com/solarized
% % % % % % % % % % % % % % % %

\endinput
