\name{restricted_models}
\alias{restricted_models}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find All Restricted Models from a Regression Formula
}
\description{
Currently, this has the shortcoming of not properly handling three-way or higher order interactions. We should exclude models that have a three-way interaction without the corresponding two-way interactions, but this function does not do this. I will let this be for now.

The function has another shortcoming, viz. that the same random effects are added to each model, without checking whether they make sense.

These shortcomings could generally be described as not respecting the property of marginality in all cases.
}
\usage{
restricted_models(fmla)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fmla}{
%%     ~~Describe \code{fmla} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (fmla)
{
    model_parts = parse_formula(fmla)
    n_fixed = length(model_parts$fixed)
    all_fixed = model_parts$fixed
    combo_list = list()
    for (m in n_fixed:1) {
        combo_list = c(combo_list, combn(all_fixed, m = m, simplify = FALSE))
    }
    to_remove = c()
    for (i in 1:length(combo_list)) {
        is_interaction = grepl("\\:", combo_list[[i]])
        if (any(is_interaction)) {
            interactions = combo_list[[i]][is_interaction]
            mains = combo_list[[i]][!is_interaction]
            n_interactions = length(interactions)
            for (j in 1:n_interactions) {
                needed_mains = strsplit(interactions[j], "\\s*\\:\\s*")[[1]]
                has_needed_mains = all(needed_mains \%in\% mains)
                if (!has_needed_mains) {
                  to_remove = c(to_remove, i)
                }
            }
        }
    }
    if (length(to_remove) > 0) {
        combo_list = combo_list[-to_remove]
    }
    combo_list[[length(combo_list) + 1]] = c("1")
    formula_strings = c()
    formula_list = list()
    for (i in 1:length(combo_list)) {
        rhs = paste(c(combo_list[[i]], model_parts$random), collapse = " + ")
        formula_strings[i] = paste(c(model_parts$lhs, rhs), collapse = " ~ ")
        formula_list[[i]] = formula(formula_strings[i])
    }
    omitted = list()
    omitted[[1]] = NULL
    for (i in 2:length(combo_list)) {
        omitted[[i]] = setdiff(all_fixed, combo_list[[i]])
    }
    result = list(formula_strings = formula_strings, formulas = formula_list,
        included = combo_list, omitted = omitted)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
