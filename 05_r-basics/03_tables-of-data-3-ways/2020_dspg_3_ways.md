"The 3 Ways of R" or Becoming a Better Code Googler (GoogleR?)
========================================================
author: Aaron D. Schroeder
date: 6/5/2020
autosize: true

<style>
  
  /* slide titles */
  .reveal h3 { 
    font-size: 100px;
    color: blue;
  }

/* heading for slides with two hashes ## */
.reveal .slides section .slideContent h2 {
  font-size: 70px;
  font-weight: bold;
  color: green;
  line-height: 1.2;
}

/* heading for slides with two hashes ### */
.reveal .slides section .slideContent h3 {
  font-size: 50px;
  font-weight: bold;
  color: green;
  line-height: normal;
}

/* ordered and unordered list styles */
  .reveal ul, 
.reveal ol {
  font-size: 60px;
  color: red;
  line-height: 1.6;
}

</style>

The R Langauge - Multiple Approaches
========================================================
# Used many different ways, including

- **Procedural programming (PP)**
  - HOW MOST EVERYBODY STARTS, also known as inline programming takes a top-down approach. Writing a list of instructions step by step.
- **Object-oriented programming (OOP)**
  - encapsulating data and behavior into objects. An OOP application will use a collection of objects which knows how to perform certain actions and how to interact with other elements of the application.
- **Functional programming (FP)**
  - WHAT MOST PEOPLE GET TO AFTER PROCEDURAL, passing data from function to function to function to get a result. Functions are treated as data, meaning you can use them as parameters, return them, build functions from other functions, etc.

Like Javascript
========================================================
### Can be also be used as PP, OOP, or FP
### 'Frameworks' are created to make it easier
- **React**
- **Vue.js**
- **Ember.js**
- **Meteor.js**

### And many more! BUT, A Single Framework can Lock You In!

### PROBLEM! People are only learning how to do Javascript the "React" or "Vue.js" or ... way - VERY LIMITING - ESPECIALLY FOR USING OTHER PEOPLE'S CODE!

R 'Frameworks'
========================================================
### While not exactly "Frameworks", R has the same issue in that different coding "approaches" have emerged

- **base**
  - championed purists who like to do things the old-fashioned way (reminds them of the good-ol-days before S was renamed to R)
  - if you know what is going on "under the hood" you can write susinct and fast code, but if you don't...
- **tidy** (tibl, dplyr, stringr, many many more)
  - championed by the 'followers of the Wickham Way', I'm no going to say it's a cult, but...
  - an "opionated" way to do functional programming that is easier to pick up than base
  - get all data to tidy format, then process in an easy to follow chain
  - way different than base and rather verbose
- **datatable**
  - championed by grumpy programmers who value speed and sussinctness most
  - actually just a very fast data structure that can be used with tidy or anything else
  - however, has it's own syntax that can be significantly more susinct
- **hybrid**
  - combine the speed of data.table with the programming style of tidy
  - more and more packages are appearing to make this possible

Need to be Bi/Tri Lingual!
========================================================
<style>
.reveal H1 {
  line-height: normal;
</style>
# If you are going to be a great R Code Googler (which you want to be!) you need to be able to at least read and understand what is going on in the code samples you find, regardless of the approach used!

Today's Objectives
========================================================
<style>
.reveal ul, 
.reveal ol {
  font-size: 60px;
  color: red;
  line-height: 1.1;
</style>
# So, for today our objectives are to

- Get an introduction to very simple versions of the different approaches to
  - reading data into R
  - susbsetting data (selecting & filtering)
  - renaming columns
  - piping
- Send you off to the interactive DataCamp tutorials to learn more and practice 
  - Data Manipulation with dplyr
  - Data Manipulation with data.table
