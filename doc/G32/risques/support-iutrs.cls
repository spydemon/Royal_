\NeedsTeXFormat{LaTeX2e}

\ProvidesClass{support-iutrs}[2011/08/27 Classe support de cours IUT RS, V1.0]

\LoadClass[a4paper, 11pt]{article}

%%% Extensions
\RequirePackage{a4wide}
\RequirePackage[utf8]{inputenc}
\RequirePackage[T1]{fontenc}
\RequirePackage[french]{babel}
\RequirePackage{latexsym}   
\RequirePackage{amssymb}
\RequirePackage{amsmath}
\RequirePackage{epic,eepic,epsfig}
\RequirePackage[usenames]{color}
\RequirePackage{ifthen}
\RequirePackage{fancyheadings}
\RequirePackage{listings}
\RequirePackage{mdwlist}
\RequirePackage{url}
\RequirePackage{xcolor}
%\RequirePackage{hyperref}
\RequirePackage[textwidth=17mm]{todonotes}
\RequirePackage{verbatim}
\RequirePackage{moreverb}
\RequirePackage{tabularx}
\RequirePackage{pslatex,eso-pic}
\RequirePackage{fancyhdr,lastpage}
\RequirePackage{lastpage}
\pagestyle{fancy}


%%% Gestion des marges
\RequirePackage{fullpage}
\addtolength{\topmargin}{15pt}
\addtolength{\textheight}{40pt}

%\setlength{\hoffset}{-18pt}  	
%\setlength{\oddsidemargin}{0pt} 	% Marge gauche sur pages impaires
%\setlength{\evensidemargin}{9pt} 	% Marge gauche sur pages paires
%\setlength{\marginparwidth}{54pt} 	% Largeur de note dans la marge
%\setlength{\textwidth}{481pt} 	% Largeur de la zone de texte (17cm)
%\setlength{\voffset}{0pt} 	% Bon pour DOS
%\setlength{\marginparsep}{7pt} 	% Séparation de la marge
%\setlength{\topmargin}{18pt} 	% Pas de marge en haut
%\setlength{\headheight}{10pt} 	% Haut de page
%\setlength{\headsep}{2pt} 	% Entre le haut de page et le texte
%\setlength{\footskip}{27pt} 	% Bas de page + séparation
%\setlength{\textheight}{670pt} 	% Hauteur de la zone de texte (25cm)

%%% Gestion des espaces
\parskip=5pt

\def\LogoPath{figs/logos}
\newcommand{\logospath}[1]{\def\LogoPath{#1}}
%%% Commandes personnelles 

% prof
\newcommand{\prof}[2][]{}
\DeclareOption{prof}{
	\renewcommand{\prof}[2][inline, caption={prof}]{\todo[color=blue!10,#1]{\sf \textbf{Prof :} #2}}
}
\ProcessOptions




% code
\newcommand{\code}[1]{\texttt{#1}}
%\newcommand{\code}[1]{\begin{tt}{#1}\end{tt}}
%\newenvironment{code}{\begin{listing}{0}}{\end{listing}}
%\newcommand{\inputcode}[1]{\listinginput{0}{#1}}




% header
\newcommand{\infos}[3]{
	\def\Mail{#1}
	\def\NomUE{#2}
	\def\CodeUE{#3}
}

\newcommand{\sujet}[1]{\def\Sujet{#1}}
\newcommand{\titre}[1]{\def\Titre{#1}}
\newcommand{\objectif}[1]{\def\Objectif{#1} \def\ObjNot{X}}
\newcommand{\notions}[1]{\def\Notions{#1} \def\ObjNot{X}}

\newcolumntype{R}{>{\raggedleft\arraybackslash}X}%
\newcommand{\header}{
\vspace*{-3cm}
\noindent
\begin{tabularx}{\textwidth}{XcR}
\noindent\includegraphics[height=1.3cm]{\LogoPath/logo_uds} &
\hfill \shortstack[l]{\sc \Huge \CodeUE\ \Sujet \\\\\NomUE\\\\ \Mail} &
\includegraphics[height=1.3cm]{\LogoPath/logo_iut} \\
\end{tabularx}


\ifdefined\Titre
	\begin{center}
	\huge\bf\Titre
	\end{center}
\fi


\ifdefined\ObjNot
\noindent\rule[0mm]{\textwidth}{0.1mm} %\rule[lift]{width}{height}

\large
\ifdefined\Objectif
	\noindent
	\emph{Objectif} : \Objectif
\fi

\ifdefined\Notions
	\noindent
	\emph{Notions} : \Notions
\fi

\noindent\rule[2mm]{\textwidth}{0.1mm} %\rule[lift]{width}{height}
\fi

\normalsize
}
