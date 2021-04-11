
# RUn before test

devtools::load_all()

# Run this before committing changes to github

# Step 1 - restart your session

devtools::document()
devtools::test()
devtools::check()
