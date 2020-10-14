# Topic Modeling in R
The following tutorial builds on chapter 6 in [*Text Mining with R*](https://www.tidytextmining.com/topicmodeling.html). Additionally, this tutorial draws on the `tm` and `ldatuning` packages. See the following links for sources referenced when writing the code for this tutorial:
- <https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf>
- <https://mran.microsoft.com/snapshot/2016-08-05/web/packages/ldatuning/vignettes/topics.html>

This tutorial utilizes the LDA topic model to analyze topics across a corpus of documents. The following video introduces LDA and some potential uses:
- <https://youtu.be/FkckgwMHP2s>

Goals:
- Clean data using TM package
- Extract per topic probabilities
- Visualize 2 topics
- Visualize words with greatest differences between 2 topics
- Infer and visualize number of topics intuitively
- Calculate and visualize number of topics empirically
