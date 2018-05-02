all_inputs <- sapply(RatesFitsBest, function(x) names(x$trainingData))
all_outputs <- names(all_inputs)
gloss <- unique(c(all_inputs, all_outputs))
gloss <- gloss[gloss != ".outcome"]
gloss <- cbind(
  "Code name (raw)" = gloss,
  "Code name" = gloss,
  "Description" = c(
    "Gender == Male",
    "Gender == Female",
    "Education in Years",
    "Left-handed",
    "Grandparent with PD",
    "Parent with PD",
    "Age at Baseline",
    "Time of diagnosis at baseline",
    "UPDRS I score at baseline",
    "UPDRS II score at baseline",
    "UPDRS III score at baseline",
    "Hoehn and Yahr Stage",
    "Modified Schwab and England - Overall",
    "Neuropsyc. Test (Line Orientation) - Sum 15 item",
    "Epworth Sleepiness Scale - Total",
    "Geriatric Depression Scale - Total",
    "Derived-Total Recall T-Score",
    "Derived-Delayed Recall T-Score",
    'Derived-Retention T-Score',
    'Derived-Recog. Discrim. Index T-Score',
    'Derived-(Letter Number Sequencing) Scaled Score',
    'Questionnaire for Impulsive-Compulsive Disorders in PD - Total',
    'REM Sleep Disorder Questionnaire - Total',
    "SCales for Outcomes in PD-PsychoSocial questionnaire - Total",
    'Derived-Sem. Fluency-Animal Scaled Score',
    'Montreal Cognitive Assessment - Total',
    'State-Trait Anxiety Inventory - Total',
    'Derived-Symbol Digit T-Score',
    'University of Pennsylvania Smell ID Test - Total',
    'Supine Blood Pressure - systolic',
    'Supine Blood Pressure - diastolic',
    'Supine heart rate',
    'Standing Blood Pressure - systolic',
    'Standing Blood Pressure - diastolic',
    'standing heart rate',
    'Body Mass Index',
    'Asymmetry Index for Putamen',
    'Asymmetric Index for Caudate',
    'Mean Caudate / Mean Putamen',
    'Mean Striatum (right and left)',
    'Mean Putamen (right and left)',
    'Mean Caudate (right and left)',
    'SI Result for Albumin-QT',
    'SI Result for Alkaline Phosphatase',
    'SI Result for ALT (SGPT)',
    'SI Result for APTT-QT',
    'SI Result for AST (SGOT)',
    'SI Result for Basophils',
    'SI Result for Basophils (%)',
    'SI Result for Calcium (EDTA)',
    'SI Result for Creatinine (Rate Blanked)',
    'SI Result for Eosinophils',
    'SI Result for Eosinophils (%)',
    'SI Result for Hematocrit',
    'SI Result for Hemoglobin',
    'SI Result for Lymphocytes',
    'SI Result for Lymphocytes (%)',
    'SI Result for Monocytes',
    'SI Result for Monocytes (%)',
    'SI Result for Neutrophils',
    'SI Result for Neutrophils (%)',
    'SI Result for Platelets',
    'SI Result for Prothrombin Time',
    'SI Result for RBC',
    'SI Result for Serum Bicarbonate',
    'SI Result for Serum Chloride',
    'SI Result for Serum Glucose',
    'SI Result for Serum Potassium',
    'SI Result for Serum Sodium',
    'SI Result for Serum Uric Acid',
    'SI Result for Total Bilirubin',
    'SI Result for Total Protein',
    'SI Result for Urea Nitrogen',
    'SI Result for WBC',
    'Test Value for Abeta 42',
    'Test Value for CSF Alpha-synuclein',
    'Test Value for p-Tau181P',
    'Test Value for Total tau',
    'Test Value for SCORE',
    'Test Value for Serum IGF-1',
    'Apolipoprotein E (APOE) gene (e3/e2)',
    'Apolipoprotein E (APOE) gene (e3/e3)',
    'Apolipoprotein E (APOE) gene (e4/e3)',
    'How many G in rs55785911 (DNA)',
    'How many G in rs12456492 (DNA)',
    'How many T in rs17649553 (DNA)',
    'How many G in rs11868035 (DNA)',
    'How many G in rs14235 (DNA)',
    'How many G in rs2414739 (DNA)',
    'How many T in rs11158026 (DNA)',
    'How many G in rs11060180 (DNA)',
    'How many T in rs76904798 (DNA)',
    'How many T in rs329648 (DNA)',
    'How many G in rs591323 (DNA)',
    'How many T in rs199347 (DNA)',
    'How many T in rs115462410 (DNA)',
    'How many T in rs8192591 (DNA)',
    'How many T in rs3910105 (DNA)',
    'How many T in rs356181 (DNA)',
    'How many T in rs6812193 (DNA)',
    'How many G in rs11724635 (DNA)',
    'How many G in rs34311866 (DNA)',
    'How many G in rs34884217 (DNA)',
    'How many G in rs12637471 (DNA)',
    'How many G in rs1955337 (DNA)',
    'How many T in rs6430538 (DNA)',
    'How many T in rs10797576 (DNA)',
    'How many T in rs823118 (DNA)',
    'p-Tau181P / Total tau',
    'p-Tau181P / Abeta 42',
    'Total tau / Abeta 42'
  )
)

gloss[,2] <- gsub("\\`","",gloss[,1])
gloss[,2] <- gsub("as.numeric\\(","",gloss[,2])
gloss[,2] <- gsub("\\)","",gloss[,2])


gloss <- gloss[order(gloss[,3]),]

refs <- c(
  "Berendse, H. W., Booij, J., Francot, C. M. J. E., Bergmans, P. L. M., Hijman, R., Stoof, J. C. and Wolters, E. Ch. (2001), Subclinical dopaminergic dysfunction in asymptomatic Parkinson's disease patients' relatives with a decreased sense of smell. Ann Neurol., 50: 34-41. doi:10.1002/ana.1049",
  "Boser, B., Guyon, I., Vapnik, V. (1992). A training algorithm for optimal margin classifiers.
  Proceedings of the fifth annual workshop on Computational learning theory - COLT /92. p. 144
  doi: 10.1145/130385.130401",
  "Brieman, L. (2001). Random Forests. Machine Learning, 45(1):5-32.
  doi: 10.1023/A:1010933404324.",
  "Brieman, L. (1997). Arcing The Edge. Technical Report 486, Statistics Department, University of
  California, Berkeley, CA.",
  "Cortes, C., Vapnik, V. (1995). Support-Vector Networks. Machine Learning. 20(3):273-297.
  doi: 10.1007/BF00994018.",
  "Friedman, J., Hastie, T., Tibshirani, R. (2010). Regularization Paths for Generalized Linear Models
  via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22. url:
  http://www.jstatsoft.org/v33/i01/.",
  "Friedman, J. (1999). Stochastic Gradient Boosting. url:
  https://statweb.stanford.edu/~jhf/ftp/stobst.pdf.",
  "Fahn S, Elton RL. UPDRS Development Committee. The Unified Parkinson's Disease Rating Scale.
  In: Fahn S, Marsden CD, Calne DB, Goldstein M, editors. Recent Developments in Parkinson's Disease.2nd edn Macmillan Healthcare Information; Florham Park, NJ: 1987. pp. 153-163.pp. 293-304.",
  "Friedman, J. (1999). Greedy Function Approximation: A Gradient Boosting Machine. IMS 1999 Reitz Lecture. url: http://statweb.stanford.edu/~jhf/ftp/trebst.pdf.",
  "Hastie, T., Tibshirani, R., Friedman, J. (2009). Elements of Statistical Learning. New York: Springer.",
  "Karatzoglou, A., Smola, A., Hornik, K., Zeileis, A. (2004). kernlab -- An S4 Package for Kernel Methods in R. Journal of Statistical Software, 11(9):1-20. url: http://www.jstatsoft.org/v11/i09/.",
  "Kuhn, M., Johnson, K. (2013). Applied Predictive Modeling. New York: Springer. Liaw, A., Wiener, M. (2002). Classification and Regression by randomForest. R News, 2(3):18-22. url: http://CRAN.R-project.org/doc/Rnews/.",
  "Mason, L., Baxter, J., Bartlett, P., Frean, M. (1999). Boosting Algorithms as Gradient Descent.
  Advances in Neural Information Processing Systems 12. MIT Press. Pp. 512-518.",
  "Ripley, B. (1996). Pattern Recognition and Neural Networks. Cambridge, UK: Cambridge
  University Press.",
  "PPMI Annual Investigator Meeting. (2016). Parkinson's Progression Marker Initiative. Michael J. Fox Foundation. New York, NY. May 4-5, 2016. url: http://www.ppmi-info.org/wp-content/uploads/2016/05/2016-PPMI-annual-mtg_DAY-1.pdf.",
  "Venables, W., Ripley, B. (2002). Modern Applied Statistics with S. Fourth Edition. New York: Springer. url: http://www.stats.ox.ac.uk/pub/MASS4. ISBN: 0-387-95457-0",
  "Weintraub, D., Simuni, T., Caspell-Garcia, C., Coffey, C., et al. (2015). Cognitive Performance and Neuropsychiatric Symptoms in Early, Untreated Parkinson's Disease. Movement Disorders, 30:919-927. doi: 10.1002/mds.26170.",
  "Wold, H. (1966). Estimation of principal components and related models by iterative least squares. In P.R. Krishnaiaah (Ed.). Multivariate Analysis, pp. 391-420. New York: Academic Press.",
  "Wold, S., Sjostrom, M., Ericksson, L. (2001). PLS-regression: a basic tool of chemometrics. Chemometrics and Intelligent Laboratory Systems, 58(2):109-130. doi:10.1016/S01697439(01)00155-1."
)

save(gloss, refs, file = "glossary.RData")
