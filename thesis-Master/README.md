## Description

The Master thesis is an project conducted independently or with the supervision of a company, in order to find solutions for a business analytics challenge. 

My thesis worked on the online assortment optimization on a two-sided platform - a common design for e-commerce platform connecting users/consumers with suppliers who provides service (e.g Fiverr, freelance work platforms, etc.) The platform is characterized by preferences coming from both sides of platform, and the platform needs to maximize outcome by selecting an appropriate assortment (first/second/third/N page results) to show to the side of customers.
i
After training the algorithm and perform testing on a simulation, it is shown that my algorithm can increase the outcome in revenue by ~0.5-0.9%, compared to manual ranking formula. It also does a slightly better job in matching potential students and retaining them with suitable tutors for longer amount of time.

## Highlights

  - Condensed queries from raw data sources that transform into statistics about tutor and student (for the purpose of prediction). For each pair of tutor-student, the stats are only collected up until their unique timestamps (in other words, temporal data for each data point).
  - Machine learning algorithms used and tested extensively, using Python & accompanying packages (most notably scikit-learn)
  - Exportation of the model artifacts, training data, test samples and parameters - making the model ready to be deployed in AWS Sagemaker and on the website, using AWS Sagemaker & REST API.
  
## Access

A dedicated repo stores the materials for the thesis, which can be accessed [through this link](https://github.com/khanhtrue253/master-thesis)