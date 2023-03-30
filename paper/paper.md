---
title: 'predictNMB: An R package to estimate if or when a clinical prediction model is worthwhile'
tags:
  - R package
  - clinical prediction model
  - net monetary benefit
  - cutpoint
  - clinical decision making
authors:
  - name: Rex Parsons
    orcid: 0000-0002-6053-8174
    affiliation: 1
  - name: Robin D. Blythe
    orcid: 0000-0002-3643-4332
    affiliation: 1
  - name: Adrian G. Barnett
    orcid: 0000-0001-6339-0374
    affiliation: 1
  - name: Susanna M. Cramb
    orcid: 0000-0001-9041-9531
    affiliation: "1, 2"
  - name: Steven M. McPhail
    orcid: 0000-0002-1463-662X
    affiliation: "1, 3"
affiliations:
 - name: Australian Centre for Health Services Innovation and Centre for
         Healthcare Transformation,
         School of Public Health and Social Work,
         Faculty of Health,
         Queensland University of Technology,
         Kelvin Grove,
         Australia
   index: 1
 - name: Jamieson Trauma Institute,
         Royal Brisbane and Women’s Hospital,
         Metro North Health,
         Herston,
         Australia
   index: 2
 - name: Clinical Informatics Directorate,
         Metro South Health,
         Woolloongabba,
         Australia
   index: 3
date: 31 March 2023
bibliography: paper.bib
---

# Summary 

Clinical prediction models are frequently developed for identifying patients at
risk of adverse health events, and possibly guiding the use of treatment, but
are often not validated or implemented in clinical practice 
[@hendriksen2013diagnostic; @steyerberg2013prognosis]. This could be due to
several factors including poor performance or the lack of an effective
intervention that can be implemented in response to prediction of high risk.
The ``predictNMB`` R package performs simulations to evaluate the use of 
hypothetical clinical prediction models (with a binary outcome) to help inform
development and implementation decisions, and estimate potential impacts in 
terms of costs and health outcomes. This package allows the user the flexibility
to adjust simulation inputs regarding the prediction model’s performance, its 
target population, the costs of the event being predicted, and the effectiveness
of interventions that the model is being used to recommend. More details about 
the package, including guides and a detailed example are available on the 
[package site](https://docs.ropensci.org/predictNMB/).

# Statement of need

Clinical decision support systems are often used to classify patients into 
high- or low-risk groups and to recommend treatment assignment 
[@steyerberg2013prognosis]. These systems can only perform as well as the 
underlying model(s) informing decision support recommendations, the treatments 
being recommended, and the implementation of the system within clinical 
settings. Often, the cost-effectiveness of these systems is not known until 
they are developed, implemented, and evaluated  [@reilly2006translating; @10.1093/jamia/ocad040]. 
The ``predictNMB`` R package aims to avoid this delay by facilitating early 
estimation of the cost-effectiveness of these systems. We expect most users to
be either: 1) those involved in health service decision making regarding 
investment in development or implementation of clinical decision support 
systems, or 2) clinical prediction model developers, who may be deciding whether
to invest efforts into clinical prediction model development or validation.
Characteristics of the user's given patient population are incorporated using
Monte Carlo simulation to estimate the expected cost-effectiveness of a given
system (under an assumption of ideal implementation and complete adherence to
recommendations) to provide guidance on cost-effectiveness before prediction
models are developed or implemented. For example, by evaluating this simulated 
decision support system and finding that a clinical prediction model would only
be effective (better than a treat-all or treat-none approach) at an 
unrealistically high level of model performance, users would then have 
opportunity to reduce research waste by avoiding model development, 
implementation, and evaluation in a clinical setting. Similar to a statistical 
power analysis, ``predictNMB`` allows users to estimate how well their model
would need to perform, and its expected benefit, if implemented to offer a
treatment recommendation. It may be found that a given decision support system
may only be likely to improve care when the available treatment is of a certain
level of effectiveness or when the prevalence of the condition is relatively
low or high, and this may better guide the user regarding which treatment the
system should be recommending, or for which patients. 

# Features

``predictNMB`` simulates well-calibrated prediction models using logistic
regression and incorporates a range of inbuilt cutpoint selection methods,
including a treat-all (cutpoint=0) and treat-none (cutpoint=1) method, and two
methods that aim to maximize the Net Monetary Benefit (NMB): 'cost-minimizing'
[@wynants2019three] and 'value-optimizing'[@parsons2023cutpoints]. It also 
allows the user to specify any other function for cutpoint selection. Evaluation
of the models in terms of the NMB requires the user to pass information 
regarding the costs associated with each of four possible classifications.
A helper function is provided to make this process easier by taking arguments
in terms of treatment effectiveness and outcome costs, along with their 
uncertainty (see creating [NMB functions vignette](https://docs.ropensci.org/predictNMB/articles/creating-nmb-functions.html)
for more details). 

The simulations are stored as one of two types of objects, depending on whether 
a single scenario was used for simulation or if a range of values were screened
over several simulation scenarios. Plotting and summarizing methods for these
objects are exported to easily visualize and evaluate the results of the 
simulation study (see [summarising results vignette](https://docs.ropensci.org/predictNMB/articles/summarising-results-with-predictNMB.html)
for more details).

A detailed example of a pressure injury model using inputs from the literature
is included as a [vignette](https://docs.ropensci.org/predictNMB/articles/detailed-example.html).
Applying ``predictNMB`` for this use case indicates that, when using realistic 
values for the intervention and prevalence of pressure injuries and their costs,
the clinical prediction model may be useful when the model is particularly 
well-performing (area under the receiver operator characteristic curve  > 0.8)
and when the event rate for pressure injuries is lower (event rate of 0.05).
When the event rate was higher, the treat-all strategy was preferred to any of
the model-guided approaches or treating none. This suggests that model 
development and implementation efforts should target patient populations where
the event rate of pressure injuries is lower than 0.05.

# Acknowledgements

This work was supported by the Digital Health Cooperative Research Centre
(“DHCRC”). DHCRC is funded under the Commonwealth’s Cooperative Research Centres
(CRC) Program. SMM and SMC are supported by NHMRC-administered fellowships 
(#1181138 and #2008313, respectively).

# References
