# Lexer and Parser

Please put your Lexer.lex and Parser.cup files into the src subdirectory.

To build, issue `make`.

To test, issue `make test`.

To run on a single test file, issue `./bin/sc tests/open/<some test>.s`

# Instructions:

Create a file called group.txt in the top level directory. This file should contain the names, student IDs, and (optionally) email addresses of everybody in your group, like this:

  849159 Raj Reddy
  479195 <m.hamilton@nasa.gov> Margaret Hamilton
  849159 <flow-matic@rand.com> Grace Hopper
  985704 <e.dijkstra@tue.nl> Edsger W. Dijkstra

Note that at least one group member must supply an email address; your pushes will be rejected otherwise. A report, containing details of the test runs, will be sent to every email address in group.txt whenever you push the repository.
Add and `commit group.txt` to the repository.

Your repository at UCL can be accessed over SSH using the following address:
**USER@gitlab.cs.ucl.ac.uk:/cs/student/comp207p/comp207p_GROUP**
where `USER` is your CS username and `GROUP` is your two-digit long group number. If your group number is one digit long, then ***zero-pad it, like comp207p_07***.

Add your UCL repository as a remote, like this:
  git remote add ucl kkhazem@gitlab.cs.ucl.ac.uk:/cs/student/comp207p/comp207p_88
and then push to that repository:
  git push ucl master
If all goes well, you should soon get an emailed report informing you how well your compiler did. This is the first year that we have deployed this system, so there may well be bugs; thanks a lot for your patience! Please let me know if you have any questions or concerns.
Remember that Git is a distributed VCS, so the UCL remote doesn't have to be your `master` copy. In particular, you can do most of your development using a different remote (e.g. GitHub) and only push to UCL when you want to test your compiler. Or you can push to UCL every time, up to you.
