En aquest fitxer s'explica com s'han d'executar els codis per tal de seguir les indicacions de la documentació.

Els codis es troben a la carpeta src, les dades en la carpeta data, i podem trobar alguns models entrenats en la carpeta models.

En primer lloc trobem la part del pre-processat de dades, per tal de realitzar-lo executem exploration.R

Després, executem els següents scripts que ens permetran fer les diferents proves. Alguns d'ells els hem dividit en dos degut al cost computacional.
  - LASSO: No caldria executar LASSO.R per tal d'entrenar el model ja que el tenim guardat a la carpeta "models". En aquest cas executem LASSO_evaluation.R
  - SVM lineal: Executem svm_lin.R per tal d'entrenar el model i evaluar-ne l'error.
  - SVM quadràtic: Ídem amb el primer cas. Podem executar directament svm_quadratic_evaluation.R per carregar el model i evaluar el seu error.
  - Neural net MLP: Degut al seu alt cost computacional (24 hores), podem executar directament nnet_results.R, que carregarà el model entrenat per nnetTrain.R
  - Random Forest: Executem rf.R, que ens entrenarà i evaluarà el millor model seguint el mètode de random forest.
  
Finalment, per tal d'avaluar el millor model, executem test.R
