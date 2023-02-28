make_clean_pdf <- function(file_in, file_out) {
  tinytex::xelatex(file_in, pdf_file = file_out)
}

make_clean_pdf("hypothesis_chapter.tex", "hypothesis_chapter.pdf")
