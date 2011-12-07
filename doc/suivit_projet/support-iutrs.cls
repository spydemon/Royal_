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
\RequirePackage{multicol}



%%% Gestion des marges
\RequirePackage{fullpage}
\addtolength{\topmargin}{15pt}
\addtolength{\textheight}{40pt}

%%% Gestion des espaces
\parskip=5pt


%%% Commandes personnelles 

\def\LogoPath{figs/logos}
\newcommand{\logospath}[1]{\def\LogoPath{#1}}

% prof cours
\newcommand{\prof}[2][]{}
\DeclareOption{prof}{
	\renewcommand{\prof}[2][caption={prof}]{{\color{red}{\hfill\textbf{#2}}}}
}
\ProcessOptions

\newcommand{\profgros}[2][]{}
\DeclareOption{prof}{
	\renewcommand{\profgros}[2][inline, caption={prof}]{\todo[color=blue!10,#1]{\sf #2 }}
}
\ProcessOptions


% code
\newcommand{\code}[1]{\texttt{#1}}
%\newcommand{\code}[1]{\begin{tt}{#1}\end{tt}}
%\newenvironment{code}{\begin{listing}{0}}{\end{listing}}
%\newcommand{\inputcode}[1]{\listinginput{0}{#1}}




% header
\newcommand{\infos}[3]{
	\def\Mail{\url{#1}}
	\def\NomUE{#2}
	\def\CodeUE{#3}
}

\newcommand{\sujet}[1]{\def\Sujet{#1}}
\newcommand{\titre}[1]{\def\Titre{#1}}
\newcommand{\objectif}[1]{\def\Objectif{#1} \def\ObjNot{X}}
\newcommand{\notions}[1]{\def\Notions{#1} \def\ObjNot{X}}
\newcommand{\consignes}[1]{\def\Consignes{#1} \def\ObjNot{X}}

\newcolumntype{R}{>{\raggedleft\arraybackslash}X}%
\newcommand{\header}{
\vspace*{-3cm}
\noindent
\begin{tabularx}{\textwidth}{XcR}
\noindent\includegraphics[height=1.3cm]{\LogoPath/logo_uds} &
\hfill \shortstack[l]{\sc \huge \CodeUE\ \Sujet \\\\\NomUE\\\\ \Mail} &
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

\ifdefined\Consignes
	\noindent
	\Consignes
\fi

\noindent\rule[2mm]{\textwidth}{0.1mm} %\rule[lift]{width}{height}
\fi

\normalsize
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Gestion formulaire 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Commandes spécifiques pour les QCM
\newboolean{correction}
% true pour afficher la correction
% false pour la masquer
\setboolean{correction}{false}
\newcounter{QNumber}
\newcommand{\Question}[2][]{
	 \stepcounter{QNumber}
    \noindent\textbf{Question \theQNumber} --
	  #2~#1}
 	 \newenvironment{Reponse}{
	 \begin{list}{$\square$}{\leftmargin=4em}}{
\end{list}\vspace{1em}}
\newcommand{\Vrai}{
	\item[\ifthenelse{\boolean{correction}}{$\blacksquare$}{$\square$}]}
\newcommand{\Faux}{\item[$\square$]}
