library(tidyverse)
options(jupyter.rich_display=FALSE,
        repr.plot.width=15,
        repr.plot.height=8)

emails = read_csv("../data/email.csv")

head(emails)

dim(emails)

emails <- mutate(emails, message_label=as.factor(message_label))
class(emails$message_label)

emails %>%
  group_by(message_label) %>%
  count(name = "count")
# mutate(count = count/nrow(emails))

emails_long <- emails %>%
  gather(word, count, -message_index, -message_label) %>%
  arrange(message_index)

head(emails_long)

emails_long %>%
  group_by(word) %>%
  summarise(occurence = sum(count)) %>%
  arrange(desc(occurence)) %>%
  head(10)

emails_long %>%
  filter(message_label == "ham") %>%
  group_by(word) %>%
  summarise(occurence = sum(count)) %>%
  arrange(desc(occurence)) %>%
  head(10)

emails_long %>%
  filter(message_label == "spam") %>%
  group_by(word) %>%
  summarise(occurence = sum(count)) %>%
  arrange(desc(occurence)) %>%
  head(10)


# set.seed(123)
set.seed(2024)
split <- rsample::initial_split(emails, prop = 0.75)

X_train <- rsample::training(split)
X_test <- rsample::testing(split)

y_train <- X_train$message_label
y_test <- X_test$message_label


X_train %>%
  group_by(message_label) %>%
  count(name = "count") %>%
  mutate(count = count/nrow(X_train))


X_test %>%
  group_by(message_label) %>%
  count(name = "count") %>%
  mutate(count = count/nrow(X_test))


library(e1071)
email_mod <-
  naiveBayes(message_label ~ . - message_index,
             data = X_train,
             laplace = 1)


y_pred <- predict(email_mod, newdata = X_test, type = "class")
caret::confusionMatrix(y_pred, y_test)

# *takie pierwsze co mi przychodzi do glowy, to:*  
#   *- zastosowanie bardziej skomplikowanych metod zamiast binarnej klasyfikacji (tf-idf) chyba mogloby byc lepszym pomyslem*  
#   *- stworzenie n-gramow dwu- lub trzy-slowowych mogloby dac wiecej informacji, jakie zbitki slowne bardziej wskazuja na spam/ham*  
#   *- hiperparametryzacja, moznaby przetestowac rozne parametry np wygladzania laplaca*  
#   *- zwiekszyc dataset, chociaz 90% w 1000 wiadomosciach to dobry wynik*  
#   *- brac pod uwage np. nickname/adres email osoby wysylajacej maila albo naglowki maili*  
  
# załadowanie danych
library(bnlearn)
data(coronary)


bn_df <- data.frame(coronary)
res <- hc(bn_df)
plot(res)

res$arcs <- res$arcs[-which((res$arcs[,'from'] == "M..Work" & 
                               res$arcs[,'to'] == "Family")),]

fittedbn <- bn.fit(res, data = bn_df)
print(fittedbn$Proteins)


cpquery(fittedbn, event = (Proteins=="<3"), evidence = ((Smoking=="no") & 
                                                          (Pressure == ">140")))
