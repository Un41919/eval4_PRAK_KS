#' Fungsi untuk menghitung korelasi dan membuat plot
#'
#' Fungsi ini dapat digunakan untuk menghitung korelasi antara dua variabel menggunakan metode `cor()` atau `cor.test()`,
#' serta membuat plot scatter untuk visualisasi korelasi.
#'
#' @param data Data frame yang berisi variabel-variabel yang akan dihitung korelasinya
#' @param var1 Nama kolom pertama untuk korelasi
#' @param var2 Nama kolom kedua untuk korelasi
#' @param metode Metode yang digunakan untuk menghitung korelasi, bisa 'cor' atau 'cor.test'
#' @return Nilai korelasi atau hasil uji korelasi, tergantung metode yang dipilih, serta plot scatter
#' @export
#' @examples
#' # Menghitung korelasi dengan metode cor dan membuat plot
#' see_correlation(mtcars, "mpg", "hp", metode = "cor")
#'
#' # Menghitung korelasi dengan metode cor.test dan membuat plot
#' see_correlation(mtcars, "mpg", "hp", metode = "cor.test")
see_correlation <- function(data, var1, var2, metode = "cor") {
  # Menghitung korelasi
  if (metode == "cor") {
    korelasi <- cor(data[[var1]], data[[var2]], use = "complete.obs")
    print(paste("Nilai korelasi antara", var1, "dan", var2, "adalah", korelasi))
  } else if (metode == "cor.test") {
    hasil_korelasi <- cor.test(data[[var1]], data[[var2]])  # Hapus use parameter
    print(hasil_korelasi)
    korelasi <- hasil_korelasi$estimate
  } else {
    stop("Metode tidak valid. Pilih antara 'cor' atau 'cor.test'.")
  }

  # Membuat plot scatter untuk visualisasi korelasi
  plot <- ggplot2::ggplot(data, ggplot2::aes_string(x = var1, y = var2)) +
    ggplot2::geom_point() +
    ggplot2::labs(title = paste("Scatter Plot Korelasi antara", var1, "dan", var2),
                  x = var1, y = var2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::annotate("text", x = max(data[[var1]], na.rm = TRUE),
                      y = max(data[[var2]], na.rm = TRUE),
                      label = paste("Korelasi: ", round(korelasi, 2)),
                      hjust = 1.1, vjust = 1.1, color = "red")

  print(plot)  # Penting: print plot!
  return(korelasi)
}
