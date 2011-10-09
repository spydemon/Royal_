\NeedsTeXFormat{LaTeX2e}

\ProvidesClass{rapport-iutrs}[2011/08/27 Classe support de cours IUT RS, V1.0]

\LoadClass[a4paper, 11pt]{article}

%%% Extensions
\RequirePackage{a4wide}
%\RequirePackage[latin1]{inputenc}
%\RequirePackage[T1]{fontenc}
%\RequirePackage[french]{babel}
\RequirePackage{latexsym}   
\RequirePackage{amssymb}
\RequirePackage{amsmath}
\RequirePackage{epic,eepic,epsfig}
\RequirePackage[usenames]{color}
\RequirePackage{ifthen}
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
\setlength{\hoffset}{-18pt}  	
\setlength{\oddsidemargin}{0pt} 	% Marge gauche sur pages impaires
\setlength{\evensidemargin}{9pt} 	% Marge gauche sur pages paires
\setlength{\marginparwidth}{54pt} 	% Largeur de note dans la marge
\setlength{\textwidth}{481pt} 	% Largeur de la zone de texte (17cm)
\setlength{\voffset}{0pt} 	% Bon pour DOS
\setlength{\marginparsep}{7pt} 	% Séparation de la marge
\setlength{\topmargin}{18pt} 	% Pas de marge en haut
\setlength{\headheight}{13pt} 	% Haut de page
\setlength{\headsep}{10pt} 	% Entre le haut de page et le texte
\setlength{\footskip}{27pt} 	% Bas de page + séparation
\setlength{\textheight}{670pt} 	% Hauteur de la zone de texte (25cm)

%%% Gestion des espaces
\parskip=5pt


%%% Commandes personnelles 



\newcommand{\TODO}[2][]{}
\newcommand{\PTlogo}{}
\def\CodeUE{Code indéfini}
\def\Livrable{Livrable indéfini}
\def\Projet{Projet indéfinis}
\def\Auteurs{Auteurs indéfinis}
%%%\def\Date{Date indéfinie}
%%%\def\Version{Version indéfini}


\newcommand{\livrable}[1]{\def\Livrable{#1}}
\newcommand{\projet}[1]{\def\Projet{#1}}
\newcommand{\groupe}[1]{\def\Groupe{#1}}
\newcommand{\auteurs}[1]{\def\Auteurs{#1}}
%%%\renewcommand{\date}[1]{\def\Date{#1}}
%%%\newcommand{\version}[1]{\def\Version{#1}}

\DeclareOption{WIP}{
	\renewcommand{\TODO}[2][inline]{\todo[color=blue!10,#1]{\sf \textbf{TODO :} #2}}

	\definecolor{gray10}{gray}{0.9}
	\AddToShipoutPicture{\unitlength=1cm
	\put(1.5,1.5){\rotatebox{60}{\textcolor{gray10}{\fontsize{145}{145}
	\selectfont BROUILLON}}}
}

}
\DeclareOption{T3}{
	%%\renewcommand{\PTlogo}{\includegraphics[height=2cm]{figs/logos/icare.png}}
	\def\CodeUE{T3}
}
\ProcessOptions


\newcommand{\code}[1]{\begin{tt}{#1}\end{tt}}


\newcolumntype{R}{>{\raggedleft\arraybackslash}X}%
\newcommand{\pagedegarde}{

\begin{titlepage}
\vspace*{-3cm}
\noindent
\begin{tabularx}{\textwidth}{XcR}
\noindent\includegraphics[height=2cm]{figs/logos/logo_uds} &
%%\hfill \PTlogo \ \shortstack[l]{\sc \Huge \CodeUE \\\\ \huge \Groupe} &
\includegraphics[height=2cm]{figs/logos/logo_iut} \\
\end{tabularx}

{\centering

\vspace{2cm}
\Huge\textbf{\Livrable}

\Projet

\vspace{1cm}
\Large\Auteurs

%%\vspace{1cm}
%%\large\Date

%%v\Version


}

\vfill

%\tableofcontents
\begin{figure}[h]
\begin{center}
\includegraphics{../img/logoRoyal.png}
\end{center}
\end{figure}

\newpage
\end{titlepage}
}



\lfoot{\Groupe\ -- \Projet}
\cfoot{}
\rfoot{\thepage\ sur \pageref{LastPage}}
%\cfoot{\thepage\ sur \pageref{LastPage}}
%\rfoot{\Livrable\ v\Version}

