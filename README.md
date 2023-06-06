# Wavelet Boundary Effect Reduction

A method for Mitigating boundary effects from wavelet denoising to be used as a possible filter for trading, inspired by [The End Point Fast Fourier Transform](https://meyersanalytics.com/publications2/sp5epfft.pdf).

Often times in financial literature wavelet transforms (especially Muti-resolution Analysis or "MRA") are used misleadingly in forecasting frameworks. The same is true for other decomposition methods like EMD and FFT. FFT, for instance, assume the signal is periodic for the given sample and that the signal starts and ends at 0. This is clearly false for financial time-series and methods like reflecting the boundary, zero padding, mean padding, etc do not reduce the severe deformities in boundary coefficients caused by these assumptions. Methods like DWT-MRA and MODWT-MRA are non-causal as they require future data to generate coefficients at time t (up to t+L-1 future poits where L is the length of the wavelet filter). Aligning these coefficients in the time-domain with the original signal also requires circularly shifting the coefficianets by the phase shift of the underlying filter which introduces boundary effected coefficients at both ends of the coefficient arrays, rendering the most recent observations useless in forecasting.

This works presents one method to mitigate boundary effects through a multistage process. First an end-point flattening function is contructed to "flatten" the signal on a given sample. This transforms the original signal such that the first and last values of the sample are zero, respecting the assumption of periodicity. A sliding window function is created to end point flatten the signal one-step at a time and perform wavelet denoising at each window (an 800 period window is used here). For each window, only the last two coefficients are saved then the window is slid one step ahead until the end is reached. The difference between these last two coefficients at each step is taken and then summed sequentially to produce a smoothed signal free of edge effects, however the new filter's values are not of the same magnitude of the original signal. From this, a possible filter based trading system may be constructed.
