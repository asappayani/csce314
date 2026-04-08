1. My code does it both ways. It checks if the user has ran the code using args first. 
    If not, it'll prompt the user for to enter the paths for the two files.

2. Some limitations that I can see are that we've only accounted for simple csv type of files. There's
    also a limited amount of supported item types within this library system while real libraries have 
    many more types. 

3. BookOnTape extends LibraryItem because the Book class represents printed books with a page count.
    BookOnTape is an audio format with minutes instead of pages. While a BookOnTape is technically a 
    "Book", their fields are different so it was easier to just extend LibraryItem.

