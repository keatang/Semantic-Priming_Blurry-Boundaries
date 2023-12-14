## Semantic Priming Effects During Naturalistic Reading: Multiple Semantic-Primers or Just One?
#### Abstract
This study attempts to investigate the apparent blurry boundaries of the semantic priming effects during naturalistic reading. Data was collected on children between the ages of seven and nine (n = 278) on decoding errors made during a naturalistic passage reading task. The main goal of this study is to use Natural Language Processing (NLP) to determine if words could potentially have multiple semantic-primers or if words are only limited to just one semantic-primer. Semantic-similarity scores were calculated for each content word within each passage using one of two methods: (a) calculating semantic-similarity scores between target words and the preceding content word (paired semantic-similarity score), and (b) calculating semantic-similarity scores between target words and all preceding content words within the same sentence (coherence semantic-similarity score). Multivariate regression models were conducted to determine whether either of the two variables predict word decoding errors after controlling for word length and word frequency. Additionally, the amount of unique variance explained by each of the two variables was also computed. Both variables were predictive of word reading errors after controlling for word frequency and word length. However, coherence semantic-similarity scores seemed to explain more unique variance when compared to paired semantic-similarity scores. These results suggest that there may be multiple semantic-primers for each target word, and that there may be a blurry boundary to semantic-priming effects

#### Methods
##### Data (Not Included in Repository)
Data was collected across 278 participants, across two different samples who read different passages. Sample One had 142 participants who read an experimenter-created passage, "Toads''. All participants were between the ages of seven to nine, of varying levels of reading ability. Sample Two had 136 participants, who were counterbalanced to have read two out of four possible passages ("The Surprise", "The Mouse in the House", "Air", and "The Brain and Five Senses"). These passages came from the Qualitative Reading Inventory [5] which is an assessment of students’ skills in reading from elementary to high school. All participants were between the ages of seven to nine, of varying levels of reading ability. For both samples, a master’s student listened to recordings of participants during the oral reading task, and determined when and for which word a miscue error was made. 
A miscue was determined to be when a child mispronounced a word during a passage reading task. A zero meant that a child did not make a miscue for both samples. For Sample Two, a number between one to seven was assigned based on the type of miscue made; and for Sample One the number one was assigned to any type of miscue. For this analysis, for Sample Two, all numbers that were larger than one was assigned to be a value of one. This was necessary for the data to be analyzed alongside Sample One. Average miscue rates across all participants were then calculated for each word and will be used as our dependent variable of interest.

##### Linguistic Analysis
In total, data on 942 words were collected, which were all part of 111 sentences. Of the 942 words, 312 content words had at least one other content word come before it within the same sentences.  These 312 content words were the focus of the following analysis. The SpaCy package was used to calculate semantic-similarity scores between target words and potential primer(s).
SpaCy calculates semantic similarity by utilizing a technique called word2vec, which essentially measures semantic similarity based on how often words appear in similar contexts [8]. One important note about SpaCy’s semantic similarity is that some antonyms that appear in similar contexts (e.g., “love” and “hate”) will have a high semantic similarity score, despite being opposites in semantic meaning [14]. Even so, amongst antonyms, a semantic priming effect still exists [11]. 

##### *Paired Semantic-Similarity Scores*
To calculate paired semantic-similarity scores, semantic-similarity scores were calculated between the current target word and the most recent content word that preceded it. This was repeated across all content words within the passage. Stop words were filtered out by identifying them using SpaCy’s is_stop function. If a stop word was detected, they were assigned a paired semantic-similarity score of 0. Additionally, if a word was the first content-word within a passage (and thus, has no content-word before it), it was also assigned a paired semantic-similarity score of 0. All words that had a paired semantic-similarity score of 0 was filtered out before analysis, since they were either stop words or content words in which calculating a paired semantic-similarity score was not possible. 
For a more specific example, we can examine how the sentence “Each night the mouse went into the kitchen.” would be processed. First, the word “each” is a stop word, and thus would be assigned a paired semantic-similarity score of 0. This is followed by “night”, which is the first content word to appear in the passage, and thus, would be assigned a score of 0. The word “the” is another stop word, which would be assigned a score of 0. Then, since the word “mouse” is a content word, the semantic similarity score between “mouse” and preceding content word “night” will be calculated, which will result in a paired semantic-similarity score of 0.16 for the word “mouse”. Then since the next word “went” is also a content word, the semantic similarity score between “went” and the preceding content word “mouse” will be calculated, which will result in a paired semantic similarity score of 0.02 for the word “went”. This process was repeated throughout all passages to calculate paired semantic-similarity scores for all possible content words within the passages.

##### *Coherence Semantic-Similarity Scores*
To calculate coherence semantic-similarity scores, semantic similarity scores were calculated between the current target word and all prior content-words that preceded it within the same sentence. This was repeated across all content words within the passage. Once again, stop words were identified by using SpaCy’s is_stop function, and were assigned a coherence semantic-similarity score of 0. And, if a word was the first content-word within a sentence, it was also assigned a coherence semantic-similarity score of 0, since there are no other content words that came before it within the same sentence. Once again, all words that had a coherence semantic-similarity score of 0 were filtered out before analysis, since they were either stop words or content words in which calculating a coherence semantic-similarity score was not possible. 
For another example, we can once again examine how the sentence “Each night the mouse went into the kitchen.” would be processed. Again, the word “each” would be recognized as a stop word and would be assigned a coherence semantic-similarity score of 0. Then, since the word “night” is the first content word to appear in the sentence, it would also be assigned a score of 0. The stop word “the” would be assigned a score of 0. Then, for the content word “mouse”, the semantic similarity score between “mouse” and the one preceding content word “night” would be calculated, which would result in a coherence semantic-similarity score of 0.16 for the word “mouse”. Then, since the next word “went” is also a content word, the semantic similarity scores between “went” and the two preceding content words (“mouse” and “night”) in the sentence will be calculated. Essentially, this means that the semantic similarity between “went” and “mouse” will be calculated first, followed by the semantic similarity between “went” and “night”. Then, these semantic similarity scores are averaged, resulting in a coherence semantic-similarity score of 0.17 for the word “went”. This process was repeated throughout all passages to calculate coherence semantic-similarity scores for all possible content words within the passages.

##### *Word Frequency and Word Length*
Additionally, word frequency scores were sourced from the SUBTLEXus [3] corpus, which contains word frequency information on 74,286 words based on movie subtitles. The logarithmically transformed word frequency metric (“Lg10WF”) was used for analysis. Finally, word length was calculated for each word using Python, by measuring the length of each word with the built-in len() function. Both variables will be used as controls, since words are processed more quickly for high-frequency words [2] and words that have smaller word lengths [1]. Because of this, it is important to rule word length and word frequency effects as a possible explanation for any potential priming effects.

##### Statistical Analysis
First, the correlation between all variables was analyzed. This was done to better understand the relationship between all variables, and to determine if there was high multicollinearity between any variables. Coherence semantic-similarity scores and paired semantic-similarity scores had high multicollinearity – which was expected, since both measures are trying to capture semantic-relatedness between target words and their potential primer(s). However, both independent variables will be analyzed in separate models when determining their significance in predicting word reading miscues – and thus, they were not dropped from analysis. None of the other independent variables had a high multicollinearity (R > 0.70) with another independent variable.  
For RQ1, analysis was conducted using multivariate linear regression using R’s lm() function. Two separate regression models will be run for both the paired and coherence semantic-similarity variables to determine if either variable significantly predicts word reading miscues after controlling for word length and word frequency. The first regression model will have average miscue rates as its dependent variable, and paired semantic-similarity scores, word length, and word frequency as its independent variables (paired semantic-similarity model). The second regression model will have average miscue rates as its dependent variable, and coherence semantic-similarity scores, word length, and word frequency as its independent variables (coherence semantic-similarity model). Significance, slope, and R2 will be reported. 
For RQ2, a full model that contains all variables of interest (paired semantic-similarity, coherence semantic-similarity, word length, and word frequency) that predicts word reading miscues will be run, and differences in t-statistics between coherence and paired semantic-similarity variables will be reported. Additionally, the full model will be compared with a model that either doesn’t include coherence semantic-similarity scores, or a model that doesn’t include paired semantic-similarity scores. Then, the differences between R2 between the full model and the two models that do not have one of the variables of interest will be calculated. This would be a measure of the change in R2 when either the paired semantic-similarity or coherence semantic-similarity variable is added to the full model last. The variable that results in the largest gain in R2 when added to the full model last would be the variable that explains the most unique variance in word reading miscues out of the two variables of interest (paired vs coherence semantic similarity). 

##### References
[1]	Barton, J. J. S., Hanif, H. M., Eklinder Björnström, L., & Hills, C. (2014). The word-length effect in reading: A review. Cognitive Neuropsychology, 31(5–6), 378–412. https://doi.org/10.1080/02643294.2014.895314
[2]	Brysbaert, M., Mandera, P., & Keuleers, E. (2018). The Word Frequency Effect in Word Processing: An Updated Review. Current Directions in Psychological Science, 27(1), 45–50. https://doi.org/10.1177/0963721417727521
[3]	Brysbaert, M., & New, B. (2009). Moving beyond Kučera and Francis: A critical evaluation of current word frequency norms and the introduction of a new and improved word frequency measure for American English. Behavior Research Methods, 41(4), 977–990. https://doi.org/10.3758/BRM.41.4.977
[4]	Burke, D. M., & Yee, P. L. (n.d.). Semantic Priming During Sentence Processing by Young and Older Adults.
[5]	Caldwell, J., & Leslie, L. (2000). Qualitative reading inventory (3rd ed.). Boston, MA: Allyn & Bacon.
[6]	Collins, A. M., & Quillian, M. R. (1969). Retrieval time from semantic memory. Journal of Verbal Learning and Verbal Behavior, 8(2), 240–247. https://doi.org/10.1016/S0022-5371(69)80069-1
[7]	Currie, N. K., Francey, G., Davies, R., Gray, S., Bridges, M. S., Restrepo, M. A., Thompson, M. S., Ciraolo, M. F., Hu, J., & Cain, K. (2021). The Process and Product of Coherence Monitoring in Young Readers: Effects of Reader and Text Characteristics. Scientific Studies of Reading, 25(2), 141–158. https://doi.org/10.1080/10888438.2020.1831503
[8]	Explosion. (n.d.). Linguistic features · spacy usage documentation. Linguistic Features. https://spacy.io/usage/linguistic-features 
[9]	Foltz, P. W., Kintsch, W., & Landauer, T. K. (1998). The measurement of textual coherence with latent semantic analysis. Discourse Processes, 25(2–3), 285–307. https://doi.org/10.1080/01638539809545029
[10]	Joordens, S., & Becker, S. (n.d.). The Long and Short of Semantic Priming Effects in Lexical Decision.
[11]	Lucas, M. (2000). Semantic priming without association: A metaanalytic review. Psychonomic Bulletin & Review, 7(4), 618–630. https://doi.org/10.3758/BF03212999
[12]	Martin-Chang, S., & Levesque, K. (2015). Reading Words In and Out of Connected Text: The Impact of Context on Semantic and Orthographic Processing. Scientific Studies of Reading, 19(5), 392–408. https://doi.org/10.1080/10888438.2015.1059839
[13]	Meyer, D. E., & Schvaneveldt, R. W. (1971). Facilitation in recognizing pairs of words: Evidence of a dependence between retrieval operations. Journal of Experimental Psychology, 90(2), 227–234. https://doi.org/10.1037/h0031564
[14]	Mohammed, N. (2020). Extracting Word Synonyms from Text using Neural Approaches. The International Arab Jour-nal of Information Technology, 45–51. https://doi.org/10.34028/iajit/17/1/6
[15]	Myers, J. L., Cook, A. E., Kambe, G., Mason, R. A., & O’Brien, E. J. (2000). Semantic and Episodic Effects on Bridging Inferences. Discourse Processes, 29(3), 179–199. https://doi.org/10.1207/S15326950dp2903_1
[16]	Reynvoet, B., Brysbaert, M., & Fias, W. (2002). Semantic priming in number naming. The Quarterly Journal of Experimental Psychology Section A, 55(4), 1127–1139. https://doi.org/10.1080/02724980244000116
[17]	Spätgens, T., & Schoonen, R. (2019). Individual differences in reading comprehension in monolingual and bilingual children: The influence of semantic priming during sentence reading. Learning and Individual Differences, 76, 101777. https://doi.org/10.1016/j.lindif.2019.101777
[18]	Sperber, R. D., McCauley, C., Ragain, R. D., & Weil, C. M. (1979). Semantic priming effects on picture and word processing. Memory & Cognition, 7(5), 339–345. https://doi.org/10.3758/BF03196937
[19]	Swinney, D. A., Onifer, W., Prather, P., & Hirshkowitz, M. (1979). Semantic facilitation across sensory modalities in the processing of individual words and sentences. Memory & Cognition, 7(3), 159–165. https://doi.org/10.3758/BF03197534

##### Note! 
This repository contains all files, code, and passages used to investigate blurry boundaries in semantic priming effects. Data from participants are not publically available. If there are any questions about methodology, code, or data, the author of this repository can be reached at kenny.a.tang@vanderbilt.edu
