
library(systemfonts)

theme_set(theme_bw(base_family = "Lato"))

# STATISTICS GLOBE YOUTUBE 




# Select Last Column of Data Frame 
# OCT 25 2021
df1 = data.frame(
  x1 = letters[9:3],
  x2 = 2:8,
  x3 = 'z',
  x4 = LETTERS[1:7]
)

df1
#   x1 x2 x3 x4
# 1  i  2  z  A
# 2  h  3  z  B
# 3  g  4  z  C
# 4  f  5  z  D
# 5  e  6  z  E
# 6  d  7  z  F
# 7  c  8  z  G


# get last column vector array
last_vector = df1[ , ncol(df1)]
last_vector2 = df1[ , ncol(df1), drop= FALSE]

last_vector # horizontal
# [1] "A" "B" "C" "D" "E" "F" "G"

last_vector2 # vertical column
#   x4
# 1  A
# 2  B
# 3  C
# 4  D
# 5  E
# 6  F
# 7  G




# Force Plot Axes to Start at Zero 
# Oct 24, 2021
x = 5:10
y = 3:8

plot(x, y)

plot(x, y,
     xlim =  c(0, 10),
     ylim =  c(0, 8))


plot(x, y,
     xlim = c(0, 10),
     ylim = c(0, 8),
#  -- this makes x and y lines start at 0
     xaxs = 'i',
     yaxs = 'i'
     )






# Create Random Matrix
# Oct 23, 2021

set.seed(420)

matrixx = matrix(
  sample(1:100, 20, replace = T),
  ncol = 4
)
matrixx
#       [,1] [,2] [,3] [,4]
# [1,]    5   21   96   53
# [2,]   42   25   60   65
# [3,]   78   24   21   96
# [4,]   73   41   39   20
# [5,]   37   93   81    7



# Group Factor Levels
# Oct 22, 2021

x_factor = factor( c('a','b','c','c'))
x_factor
# [1] a b c c
# Levels: a b c

x_factor_new = x_factor
levels(x_factor_new) = c('a','b','b')
x_factor_new
# [1] a b b b
# Levels: a b





# Create Data Frame of Unequal Lengths
# Oct 21, 2021

n1 = 1:8
n2 = letters[1:5]

max_length = max( c(length(n1), length(n2)))
max_length

df3 = data.frame(
  col1 = c(n1, rep(NA, max_length - length(n1) )),
  col2 = c(n2, rep(NA, max_length - length(n2) ))
)

df3
#   col1 col2
# 1    1    a
# 2    2    b
# 3    3    c
# 4    4    d
# 5    5    e
# 6    6 <NA>
# 7    7 <NA>
# 8    8 <NA>



# Insert Character Pattern at Particular Position of String
# Oct 20, 2021

mystring = "abcdefghi"

new_string.1 = gsub(
  "^(.{5})(.*)$",         # Apply gsub
  "\\1_XXX_\\2",
  mystring
)

new_string.1
# "abcde_XXX_fghi"

function_insert = function(x, pos, insert) {       # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

my_string.2 = function_insert(x = mystring,    # Apply own function
                             pos = 5, 
                             insert = "_YYY_")
my_string.2
# "abcde_YYY_fghi"


# Remove All Whitespace in Each Data Frame Column
# Oct 19, 2021

data <- data.frame(x1 = c("a a", "  b", "c c c "),  
                   x2 = c("x x x", "y     ", "z"))
data                                                
#       x1     x2
# 1    a a  x x x
# 2      b y     
# 3 c c c       z


data_new1 <- as.data.frame(apply(data,              # Remove blanks
                                 2,
                                 function(x) gsub("\\s+", "", x)))
data_new1                                       

#    x1  x2
# 1  aa xxx
# 2   b   y
# 3 ccc   z

                     
library("stringr") 

data_new2 <- apply(data, 2, str_remove_all, " ")    
data_new2                                           




# rbind Data Frames by Column Index
# Oct 12, 2021

df5 = data.frame(
  x1 = 1:5,
  x2 = letters[10:14]
)
df6 = data.frame(
  y1 = 5:1,
  y2 = letters[14:10]
)

all_df = rbind(df5,
               # setting names avoids errors
               setNames(df6, names(df5)))
all_df
#    x1 x2
# 1   1  j
# 2   2  k
# 3   3  l
# 4   4  m
# 5   5  n
# 6   5  n
# 7   4  m
# 8   3  l
# 9   2  k
# 10  1  j




# Keep Original Row Order when Merging Data 
# Oct 10, 2021

df7 = data.frame(
  id = c(1:5, 3, 4),
  x1 = letters[1:7],
  x2 = "x"
)
df8 = data.frame(
  id = 3:6,
  y1 = letters[1:4],
  y2 = "y"
)

df_merge = merge(df7, df8)
df_merge

#   id x1 x2 y1 y2
# 1  3  c  x  a  y
# 2  3  f  x  a  y
# 3  4  d  x  b  y
# 4  4  g  x  b  y
# 5  5  e  x  c  y

data_join = dplyr::inner_join(df7, df8)
data_join

#   id x1 x2 y1 y2
# 1  3  c  x  a  y
# 2  4  d  x  b  y
# 3  5  e  x  c  y
# 4  3  f  x  a  y
# 5  4  g  x  b  y


# Divide Each Row of Matrix & Data Frame by Vector Element
# Oct 9, 2021

matrixx.2 = matrix(1:20, ncol = 4)
matrixx.2
#       [,1] [,2] [,3] [,4]
# [1,]    1    6   11   16
# [2,]    2    7   12   17
# [3,]    3    8   13   18
# [4,]    4    9   14   19
# [5,]    5   10   15   20

vektor = c(5,7,8,3)
vektor

# transpose the matrix
matrix_div = t( t(matrixx.2) / vektor)
matrix_div

#       [,1]     [,2]  [,3]     [,4]
# [1,]  0.2 0.8571429 1.375 5.333333
# [2,]  0.4 1.0000000 1.500 5.666667
# [3,]  0.6 1.1428571 1.625 6.000000
# [4,]  0.8 1.2857143 1.750 6.333333
# [5,]  1.0 1.4285714 1.875 6.666667


# Add Count & Percentage Labels on Top of Histogram Bars
# Oct 7, 2021

set.seed(420)
randnums = rnorm(300)

hist(randnums,
     labels = T,
     ylim = c(0, 60)
     )

hist(randnums,
     labels = paste0(round(hist(randnums, plot = F)$counts/length(randnums)*100,1),"%"),
     ylim = c(0, 60)
)



# Find Earliest & Latest Date
# Oct 6, 2021

mydates = c("2022-05-17", 
            "2021-07-27", 
            "2024-11-08",
            "2022-01-12", 
            "2021-08-21", 
            "2022-09-13"
            )

mydates_updated = as.Date(mydates)

date_diff = max(mydates_updated) - min(mydates_updated)
date_diff

xmas = as.Date("2021-12-25")
xmas
today = Sys.Date()

days_2_xmas = xmas - today
days_2_xmas



# Assign Value to Elements in Certain Range
# Oct 3, 2021

n = 20:30

new_n = n
new_n[new_n > 23 & new_n <= 27] = 99
new_n
# 20 21 22 23 99 99 99 99 28 29 30


# Aggregate Daily Data to Month & Year Intervals
# Oct 2, 2021

set.seed(420)
dayta = data.frame(
  date = sample( seq(as.Date("2020-04-20"),
                     by= 'day',
                     length.out = 1000),
                 100, replace = T),
  value= round(rnorm(100, 5, 2))
)

library(lubridate)

dayta_new = dayta
dayta_new$date = floor_date(dayta_new$date, 'month')




# Round Numeric Columns of Data Frame with Character & Factor Variables
# Oct 1, 2021

set.seed(420)

df9 = data.frame(
  c1 = rnorm(10),
  c2 = LETTERS[10:19],
  c3 = runif(10)
)
df9
#             c1 c2          c3
# 1  -0.16099018  J 0.521996421
# 2   0.53609687  K 0.182428342
# 3   0.63055674  L 0.094725494

df9_round = df9 %>% 
  mutate_if(is.numeric,
            round,
            digits= 1)
df9_round

#             c1 c2          c3
# 1  -0.16099018  J 0.521996421
# 2   0.53609687  K 0.182428342
# 3   0.63055674  L 0.094725494



# Create List of Installed Packages in R 
# Sep 28, 2021

my_pkgs = as.data.frame(
  installed.packages()[ , c(1, 3)]
)

head(my_pkgs)

#                     Package Version
# abind                 abind   1.4-5
# acs                     acs   2.1.4
# ada                     ada   2.0-5
# adagio               adagio   0.8.4
# addinexamples addinexamples   0.1.0
# ade4                   ade4  1.7-17




# Extract Data Frame Rows that do not Match Logical Condition
# Sep 27, 2021

df10 = data.frame(
  c1 = 9:3,
  c2 = LETTERS[9:3],
  c3 = 5
)

vektor = c('d','b','a')
df10_new = df10[! df10$c2 %in% vektor, ]
df10_new



# Test for Equality of All Vector Elements
# Sep 24, 2021

vektor.1 = 40:45
vektor.2 = rep(42,5) # repeat integer 7, 5 times

var(vektor.1) == 0 # test for variance
var(vektor.2) == 0

length( unique(vektor.1)) == 1
length( unique(vektor.2)) == 1



# Get All Factor Levels of Vector & Data Frame Column 
# Sep 20, 2021

vektor = factor( c('omega','alpha','bravo','zebra','tango','mango'))
levels(vektor)

df12 = data.frame(
  col.1 = factor(LETTERS[15:19]),
  col.2 = factor( c('w','z','z','w','w')),
  col.3 = factor( c('www','dot','com','org','io')),
  col.4 = rnorm(20, mean = 16, sd= 1)
)

levels(df12$col.1)
sapply(df12, levels, simplify = T) # loop over df to get the levels





# Find Transparent Equivalent of Color
# Sep 18, 2021

my.color = "#30b389"

# use the scales pkg
scales::show_col(my.color, labels = T, borders = 'black')

my.color.alpha = adjustcolor(my.color, alpha.f = 0.2)
scales::show_col( c(my.color, my.color.alpha),
                 labels = T, 
                 borders = 'black')


my.color.alpha.all = character()
i = NULL
for (i in 1:20) {
  my.color.alpha.all[i] = adjustcolor(
    my.color,
    alpha.f = i/20
  )
}
my.color.alpha.all

show_col(my.color.alpha.all)





# Change Space & Width of Bars in ggplot2 Barplot 
# Sep 17, 2021

df13 = data.frame(
  column.1 = LETTERS[22:26],
  column.2 = c(4,7,2,4,5)
)

ggplot2::ggplot(
  df13,
  aes(column.1, column.2) ) +
  geom_bar(stat = 'identity', 
           width = 0.5) + ggdark::dark_mode()

ggplot2::ggplot(
  df13,
  aes(column.1, column.2) ) +
  geom_bar(stat = 'identity', 
           fill= 'purple',
           width = 0.93) + ggdark::dark_mode()



# Extract & Count Unique Values in Each Column of Data Frame 
# Sep 16, 2021

df14 = data.frame(
  c.1 = 31:35,
  c.2 = c(9,3,4,6,7),
  c.3 = "R"
)
#   c.1 c.2 c.3
# 1  31   9   R
# 2  32   3   R
# 3  33   4   R
# 4  34   6   R
# 5  35   7   R
                                 
list_unique = lapply(df14, unique)
list_unique

# $c.1
# [1] 31 32 33 34 35

# $c.2
# [1] 9 3 4 6 7

# $c.3
# [1] "R"
                                 
                                 
count_unique = rapply(df14, function(x) length(unique(x)))
count_unique
# c.1 c.2 c.3 
# 5   5   1 





# Add Labels at End of Lines in ggplot2 Line Plot
# Sep 14, 2021
set.seed(420)

random_df = data.frame(
  x = 1:10,
  y= c( rnorm(10),
        rnorm(10,3,3),
        rnorm(10,10, 1.5),
        rnorm(10,6,2)
        ),
  group = rep( LETTERS[1:4], each= 10)
)

random_df

ggplot2::ggplot(
  random_df,
  aes(x= x, y= y, col= group)
)+
  geom_line() + ggdark::dark_mode()


random_df_labels = random_df
random_df_labels$label = NA

random_df_labels$label[which(random_df_labels$x == max(random_df_labels$x))] = random_df_labels$group[which(random_df_labels$x == max(random_df_labels$x))]
label = random_df_labels$label

ggplot2::ggplot(
  random_df,
  aes(x= x, y= y)
)+
  geom_line( aes(col= group)) + 
  ggrepel::geom_label_repel(
    aes(label=  label),
    nudge_x = 1,
    na.rm = T
  ) +
  ggdark::dark_mode() + 
  theme(
    legend.box.background =  element_blank(),
    legend.text = element_text(colour = NA),
    legend.title = element_text(colour = NA)
    )



# Convert Numeric Values to Month Names & Abbreviations
# Sep 4, 2021
month_nums = c(3,6,8,12,4)

month_names = month.name[month_nums]
month_names
#  "March"    "June"     "August"   "December" "April"  

month_abb = month.abb[month_nums]
month_abb
# "Mar" "Jun" "Aug" "Dec" "Apr"



                      
                      
                      
# Calculate Percentage in R
# Sep 3, 2021
set.seed(420)
x = sample(LETTERS[11:20], 50, replace = T)

head(x)
table_x = table(x)

# x
# K L N O P Q R S T 
# 3 6 5 8 7 6 7 6 2 

x_percent = table_x / length(x)
x_percent

# x
# K    L    N    O    P    Q    R    S    T 
# 0.06 0.12 0.10 0.16 0.14 0.12 0.14 0.12 0.04 




# Set Axis Breaks of ggplot2 Plot 
# Sep 2, 2021
df15 = data.frame(
  x = 1:6,
  y = c(8,3,8,2,10,3)
)

ggplot2::ggplot(
  df15,
  aes(x= x, y= y)
) +
  geom_line(color='yellowgreen') + ggdark::dark_mode()


# -- scale the x values
ggplot2::ggplot(
  df15,
  aes(x= x, y= y)
) +
  geom_line(color='yellowgreen') + 
  scale_x_continuous(breaks =  c(1:6)) +
  ggdark::dark_mode()




