# wolgok
Statistical modeling

1. ANOVA

둘 이상의 집단에서 처리의 효과를 비교하는 방법론 
-Normality, F-test, multiple comparison, fundamental sampling theorem, cochran's theorem...

2. ANCOVA

둘 이상의 집단에서 공변량의 효과를 제어하고 처리의 순수효과를 비교하는 방법론
-lsmeans, F-test, type III SS, regression...

3. Repeated measure ANOVA

반복요인이 존재할 경우 이를 고려하여 처리의 효과, 반복요인의 효과, 교호작용의 유의성을 검토하는 방법론
-Sphericity, compound symmetry, H-F/G-G adjustment, univariate/multivariate approach, repeated factor...

4. Contingency table Analysis

분할표를 이용한 연관성의 검정
-Chi-square test, Fisher's Exact test, McNemar's test, CMH test...


5. Regression :  linear model

predictor를 이용해 response 예측, 인과관계 파악
-simple regression, multiple regression, Gaussian basis regression, penalized(Ridge, Lasso) regression...

6. Generalized linear model

정규분포 이외의 분포를 가지는 response에 대한 선형모형
-Logistic, probit, poisson, cclog...

7. Advanced GLM

확장된 일반화 선형모형
-Cumulate logit model, baseline category model, GLM using Quasi - dist, log-linear model, model selection...

8. Feature selection on linear model

선형모형 내에 변수 선택 : avoid multicollinearity and overfitting
-Forward selection, backward elimination, stepwise, subset Lasso...

9. Principle Component Analysis

주성분분석을 이용한 inter covariate relation 규명 : avoid multicollinearity
-correlation, standardization, keiser's rule...

10. Non - linearity modeling and smoothing

선형모형으로 설명할 수 없는 모델링
-polynomial regression, step regression, spline, Generalized additive model(GAM)...

11. Classification method - KNN

Cross-validation 을 이용한 KNN tuning parameter 찾기
-KNN, metric, evaluation metric, k-fold cross validation...

12. Classification method - decision tree

DT를 이용한 classification, regression
-Split rule, node, leaf...

13. Classification method - Emsembles

앙상블 방법을 이용한 model power 향상
-Bagging, boosting, random forest, stacking...

14. Classification - SVM

support vector machine + Kernel trick을 이용한 분류, 회귀
-margin, hard margin, soft margin, kernel trick, metric...

15. Multivariate Data Analysis

다양한 다변량 분석기법
-PCA, Exploral factor analysis, confirmatory factor analysis, multi-dimensional scaling, clustering, canonical correlation, 
Structural Equation model, MANOVA, MANCOVA, Discriminant analysis...

16. Confirmatory factor analysis

Special cast of SEM
-Heywood, ML method, Relevance...

17. Non - parametric analysis

모수적 가정이 만족하지 않을때 사용할 수 있는 방법론
-Wilcoxon, rank, mann-whitney, regression, group comparison, Kruskal wallis...

18. Meta Analysis

여러 연구를 종합하여 이에 대한 추정 및 추론
-Der' simonian Laird, Peto, Inverse weight, funnel, Effect size, p-value...

19. Survival Analysis

중도절단이 포함된 자료에서 생존시간에 대한 추정/추론 및 모형화
-Life table, Kaplan Meier method, log-rank test, hazard based parametric model, accelerated lifetime model, Proportional hazard model...


20. Numerical Approach on computation

R을 이용한 수치계산 및 베이지안 방법 실습
-fundamental function을 이용한 UDF 설정, makov chain, matrix aljebra, expansion...

21. Generating random number : Monte-carlo simulation

분포 간의 연과성을 이용한 분포별 난수생성법
-poisson process, mixure model, uniform, box method, Accept-reject algorithm...

22. Makov chain - Monte Carlo method

MCMC를 이용한 난수추출방법 
-Metroplis-Hastings algorithm, Gibbs sampler...


