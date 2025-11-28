//
//  ParticleExperienceViewController.swift
//  PhotoMetal
//
//  UI слой, отделенный от логики рендереров. Включает два режима:
//  - Photo (2D частицы из фотографии)
//  - 3D Avatar (точечное облако, демо генерация; готово к интеграции Object Capture)
//

import UIKit
import MetalKit
import Combine

final class ParticleExperienceViewController: UIViewController,
                                              UIImagePickerControllerDelegate,
                                              UINavigationControllerDelegate {

    private enum DemoMode {
        case photo2D
        case avatar3D
    }

    // MARK: UI
    private var metalView: MTKView!
    private let headerView = UIView()
    private let footerView = UIView()
    private let brandButton = UIButton(type: .system)
    private let settingsButton = UIButton(type: .system)
    private let modeSwitcher = UIView()
    private let modePhotoButton = UIButton(type: .system)
    private let modeAvatarButton = UIButton(type: .system)

    private let slider = UISlider()
    private let sliderValueLabel = UILabel()
    private let actionPhotoButton = UIButton(type: .system)
    private let actionCameraButton = UIButton(type: .system)
    private let actionShareButton = UIButton(type: .system)
    private let triggerButton = UIButton(type: .system)
    private let scanButton = UIButton(type: .system)
    private let tutorialButton = UIButton(type: .system)
    private let photoControls = UIStackView()
    private let avatarControls = UIStackView()

    // MARK: Renderers / state
    private var photoRenderer: ParticlePhotoRenderer!
    private var avatarRenderer: ParticleAvatarRenderer?
    private var activeRenderer: ParticleAnimatable?
    private var mode: DemoMode = .photo2D
    private let parameters: ParticleEffectParameters = ParticleBranding.sharedParameters

    override func viewDidLoad() {
        super.viewDidLoad()
        view.backgroundColor = .black
        setupMetal()
        setupUI()
        setupRenderers()
        loadInitialImage()
        switchToPhotoMode()
    }

    // MARK: Setup

    private func setupMetal() {
        guard let device = MTLCreateSystemDefaultDevice() else {
            fatalError("Metal is not supported on this device")
        }
        let mtkView = MTKView(frame: view.bounds, device: device)
        mtkView.translatesAutoresizingMaskIntoConstraints = false
        mtkView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        mtkView.colorPixelFormat = .bgra8Unorm
        mtkView.preferredFramesPerSecond = 60
        mtkView.enableSetNeedsDisplay = false
        mtkView.isPaused = false
        view.addSubview(mtkView)
        NSLayoutConstraint.activate([
            mtkView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            mtkView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            mtkView.topAnchor.constraint(equalTo: view.topAnchor),
            mtkView.bottomAnchor.constraint(equalTo: view.bottomAnchor)
        ])
        metalView = mtkView
    }

    private func setupRenderers() {
        guard let device = metalView.device else { return }
        photoRenderer = ParticlePhotoRenderer(device: device, mtkView: metalView, parameters: parameters)
        if #available(iOS 17.0, *) {
            avatarRenderer = ParticleAvatarRenderer(device: device, mtkView: metalView, parameters: parameters)
        }
    }

    private func loadInitialImage() {
        if let image = UIImage(named: "example_photo") {
            photoRenderer.setImage(image)
        } else if let systemImage = UIImage(systemName: "person.fill") {
            photoRenderer.setImage(systemImage)
        }
    }

    private func setupUI() {
        headerView.translatesAutoresizingMaskIntoConstraints = false
        footerView.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(headerView)
        view.addSubview(footerView)

        let headerGradient = CAGradientLayer()
        headerGradient.colors = [UIColor.black.withAlphaComponent(0.9).cgColor,
                                 UIColor.clear.cgColor]
        headerGradient.startPoint = CGPoint(x: 0.5, y: 0)
        headerGradient.endPoint = CGPoint(x: 0.5, y: 1.0)
        headerGradient.frame = CGRect(x: 0, y: 0, width: view.bounds.width, height: 140)
        headerView.layer.addSublayer(headerGradient)

        let footerGradient = CAGradientLayer()
        footerGradient.colors = [UIColor.black.cgColor,
                                 UIColor.black.withAlphaComponent(0.7).cgColor,
                                 UIColor.clear.cgColor]
        footerGradient.startPoint = CGPoint(x: 0.5, y: 1.0)
        footerGradient.endPoint = CGPoint(x: 0.5, y: 0)
        footerGradient.frame = CGRect(x: 0, y: 0, width: view.bounds.width, height: 320)
        footerView.layer.addSublayer(footerGradient)

        NSLayoutConstraint.activate([
            headerView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            headerView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            headerView.topAnchor.constraint(equalTo: view.topAnchor),
            headerView.heightAnchor.constraint(equalToConstant: 140),

            footerView.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            footerView.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            footerView.bottomAnchor.constraint(equalTo: view.bottomAnchor),
            footerView.heightAnchor.constraint(equalToConstant: 320)
        ])

        brandButton.setImage(UIImage(systemName: "aperture"), for: .normal)
        brandButton.tintColor = .white
        settingsButton.setImage(UIImage(systemName: "ellipsis"), for: .normal)
        settingsButton.tintColor = .white
        [brandButton, settingsButton].forEach {
            $0.translatesAutoresizingMaskIntoConstraints = false
            $0.layer.cornerRadius = 20
            $0.backgroundColor = UIColor.white.withAlphaComponent(0.08)
        }

        modeSwitcher.translatesAutoresizingMaskIntoConstraints = false
        modeSwitcher.backgroundColor = UIColor(white: 1, alpha: 0.08)
        modeSwitcher.layer.cornerRadius = 22
        modeSwitcher.layer.borderWidth = 1
        modeSwitcher.layer.borderColor = UIColor.white.withAlphaComponent(0.1).cgColor

        configureModeButton(modePhotoButton, title: "Photo", isActive: true)
        configureModeButton(modeAvatarButton, title: "3D Avatar", isActive: false)
        modePhotoButton.addTarget(self, action: #selector(modePhotoTapped), for: .touchUpInside)
        modeAvatarButton.addTarget(self, action: #selector(modeAvatarTapped), for: .touchUpInside)

        let modeStack = UIStackView(arrangedSubviews: [modePhotoButton, modeAvatarButton])
        modeStack.axis = .horizontal
        modeStack.alignment = .fill
        modeStack.distribution = .fillEqually
        modeStack.spacing = 6
        modeStack.translatesAutoresizingMaskIntoConstraints = false
        modeSwitcher.addSubview(modeStack)

        headerView.addSubview(brandButton)
        headerView.addSubview(modeSwitcher)
        headerView.addSubview(settingsButton)

        NSLayoutConstraint.activate([
            brandButton.leadingAnchor.constraint(equalTo: headerView.leadingAnchor, constant: 16),
            brandButton.bottomAnchor.constraint(equalTo: headerView.bottomAnchor, constant: -24),
            brandButton.widthAnchor.constraint(equalToConstant: 40),
            brandButton.heightAnchor.constraint(equalToConstant: 40),

            settingsButton.trailingAnchor.constraint(equalTo: headerView.trailingAnchor, constant: -16),
            settingsButton.centerYAnchor.constraint(equalTo: brandButton.centerYAnchor),
            settingsButton.widthAnchor.constraint(equalToConstant: 40),
            settingsButton.heightAnchor.constraint(equalToConstant: 40),

            modeSwitcher.centerXAnchor.constraint(equalTo: headerView.centerXAnchor),
            modeSwitcher.centerYAnchor.constraint(equalTo: brandButton.centerYAnchor),
            modeSwitcher.heightAnchor.constraint(equalToConstant: 44),
            modeSwitcher.widthAnchor.constraint(equalToConstant: 220),

            modeStack.leadingAnchor.constraint(equalTo: modeSwitcher.leadingAnchor, constant: 6),
            modeStack.trailingAnchor.constraint(equalTo: modeSwitcher.trailingAnchor, constant: -6),
            modeStack.topAnchor.constraint(equalTo: modeSwitcher.topAnchor, constant: 4),
            modeStack.bottomAnchor.constraint(equalTo: modeSwitcher.bottomAnchor, constant: -4)
        ])

        // Slider
        slider.minimumValue = 0
        slider.maximumValue = 100
        slider.value = parameters.explosionStrength / 3.0 * 100
        slider.addTarget(self, action: #selector(sliderChanged), for: .valueChanged)
        slider.translatesAutoresizingMaskIntoConstraints = false
        sliderValueLabel.font = UIFont.monospacedDigitSystemFont(ofSize: 12, weight: .medium)
        sliderValueLabel.textColor = .secondaryLabel
        sliderValueLabel.text = "\(Int(slider.value))%"
        sliderValueLabel.translatesAutoresizingMaskIntoConstraints = false

        let sliderIcon = UIImageView(image: UIImage(systemName: "sparkles"))
        sliderIcon.tintColor = .secondaryLabel
        let sliderStack = UIStackView(arrangedSubviews: [sliderIcon, slider, sliderValueLabel])
        sliderStack.axis = .horizontal
        sliderStack.spacing = 12
        sliderStack.alignment = .center
        sliderStack.translatesAutoresizingMaskIntoConstraints = false

        // Action buttons row
        configureActionButton(actionPhotoButton, title: "Photos", systemName: "photo.on.rectangle.angled", selector: #selector(didTapPhotos))
        configureActionButton(actionCameraButton, title: "Camera", systemName: "camera.fill", selector: #selector(didTapCamera))
        configureActionButton(actionShareButton, title: "Share", systemName: "square.and.arrow.up", selector: #selector(didTapShare))

        triggerButton.configuration = .filled()
        triggerButton.configuration?.baseBackgroundColor = .white
        triggerButton.configuration?.baseForegroundColor = .black
        triggerButton.configuration?.image = UIImage(systemName: "bolt.fill")
        triggerButton.configuration?.imagePadding = 0
        triggerButton.configuration?.cornerStyle = .capsule
        triggerButton.addTarget(self, action: #selector(didTapTrigger), for: .touchUpInside)
        triggerButton.translatesAutoresizingMaskIntoConstraints = false
        triggerButton.widthAnchor.constraint(equalToConstant: 88).isActive = true
        triggerButton.heightAnchor.constraint(equalToConstant: 88).isActive = true

        let leftRow = UIStackView(arrangedSubviews: [actionPhotoButton, actionCameraButton])
        leftRow.axis = .horizontal
        leftRow.spacing = 12

        let rightRow = UIStackView(arrangedSubviews: [actionShareButton])
        rightRow.axis = .horizontal

        let actionRow = UIStackView(arrangedSubviews: [leftRow, triggerButton, rightRow])
        actionRow.axis = .horizontal
        actionRow.alignment = .center
        actionRow.spacing = 16
        actionRow.distribution = .equalCentering
        actionRow.translatesAutoresizingMaskIntoConstraints = false

        // 3D controls
        if #available(iOS 17.0, *) {
            scanButton.configuration = .filled()
            scanButton.configuration?.title = "Start Scanning"
            scanButton.configuration?.image = UIImage(systemName: "cube.transparent.fill")
            scanButton.configuration?.baseBackgroundColor = .white
            scanButton.configuration?.baseForegroundColor = .black
            scanButton.configuration?.cornerStyle = .capsule
            scanButton.addTarget(self, action: #selector(didTapScan), for: .touchUpInside)
            scanButton.translatesAutoresizingMaskIntoConstraints = false
        } else {
            scanButton.isHidden = true
        }

        tutorialButton.setTitle("Watch Tutorial", for: .normal)
        tutorialButton.setTitleColor(.secondaryLabel, for: .normal)
        tutorialButton.titleLabel?.font = UIFont.preferredFont(forTextStyle: .footnote)
        tutorialButton.addTarget(self, action: #selector(didTapTutorial), for: .touchUpInside)
        tutorialButton.translatesAutoresizingMaskIntoConstraints = false

        let scanStack = UIStackView(arrangedSubviews: [scanButton])
        scanStack.axis = .vertical
        scanStack.alignment = .center
        scanStack.translatesAutoresizingMaskIntoConstraints = false

        let tutorialStack = UIStackView(arrangedSubviews: [tutorialButton])
        tutorialStack.axis = .vertical
        tutorialStack.alignment = .center
        tutorialStack.translatesAutoresizingMaskIntoConstraints = false

        photoControls.axis = .vertical
        photoControls.spacing = 16
        photoControls.alignment = .center
        photoControls.translatesAutoresizingMaskIntoConstraints = false
        photoControls.addArrangedSubview(sliderStack)
        photoControls.addArrangedSubview(actionRow)

        avatarControls.axis = .vertical
        avatarControls.spacing = 16
        avatarControls.alignment = .center
        avatarControls.translatesAutoresizingMaskIntoConstraints = false
        avatarControls.addArrangedSubview(scanStack)
        avatarControls.addArrangedSubview(tutorialStack)
        avatarControls.isHidden = true
        avatarControls.alpha = 0

        footerView.addSubview(photoControls)
        footerView.addSubview(avatarControls)

        NSLayoutConstraint.activate([
            photoControls.leadingAnchor.constraint(equalTo: footerView.leadingAnchor, constant: 16),
            photoControls.trailingAnchor.constraint(equalTo: footerView.trailingAnchor, constant: -16),
            photoControls.bottomAnchor.constraint(equalTo: footerView.safeAreaLayoutGuide.bottomAnchor, constant: -16),

            avatarControls.leadingAnchor.constraint(equalTo: footerView.leadingAnchor, constant: 16),
            avatarControls.trailingAnchor.constraint(equalTo: footerView.trailingAnchor, constant: -16),
            avatarControls.bottomAnchor.constraint(equalTo: footerView.safeAreaLayoutGuide.bottomAnchor, constant: -16),

            sliderStack.widthAnchor.constraint(equalTo: photoControls.widthAnchor),
            actionRow.widthAnchor.constraint(equalTo: photoControls.widthAnchor, multiplier: 0.95),
            scanStack.widthAnchor.constraint(lessThanOrEqualTo: avatarControls.widthAnchor, multiplier: 0.9)
        ])
    }

    private func configureActionButton(_ button: UIButton, title: String, systemName: String, selector: Selector) {
        var config = UIButton.Configuration.plain()
        config.image = UIImage(systemName: systemName)
        config.imagePadding = 6
        config.title = title
        config.baseForegroundColor = .white
        config.background = UIBackgroundConfiguration.clear()
        button.configuration = config
        button.addTarget(self, action: selector, for: .touchUpInside)
    }

    private func configureModeButton(_ button: UIButton, title: String, isActive: Bool) {
        button.setTitle(title, for: .normal)
        button.titleLabel?.font = UIFont.systemFont(ofSize: 13, weight: .semibold)
        button.layer.cornerRadius = 18
        button.clipsToBounds = true
        updateModeButton(button, active: isActive)
        button.translatesAutoresizingMaskIntoConstraints = false
        button.heightAnchor.constraint(equalToConstant: 36).isActive = true
    }

    private func updateModeButton(_ button: UIButton, active: Bool) {
        if active {
            button.backgroundColor = UIColor(white: 1, alpha: 0.15)
            button.setTitleColor(.white, for: .normal)
        } else {
            button.backgroundColor = .clear
            button.setTitleColor(UIColor.white.withAlphaComponent(0.6), for: .normal)
        }
    }

    // MARK: Actions

    @objc private func modePhotoTapped() {
        switchToPhotoMode()
        updateModeButtons()
    }

    @objc private func modeAvatarTapped() {
        switchToAvatarModeIfAvailable()
        updateModeButtons()
    }

    @objc private func sliderChanged() {
        let normalized = slider.value / 100.0
        parameters.explosionStrength = max(0.1, normalized * 3.0)
        sliderValueLabel.text = "\(Int(slider.value))%"
    }

    @objc private func didTapPhotos() {
        presentImagePicker(sourceType: .photoLibrary)
    }

    @objc private func didTapCamera() {
        presentImagePicker(sourceType: .camera)
    }

    @objc private func didTapShare() {
        showAlert(title: "Share", message: "Видеошаринг пока не подключен.")
    }

    @objc private func didTapTrigger() {
        activeRenderer?.toggleDirection()
        UIImpactFeedbackGenerator(style: .medium).impactOccurred()
    }

    @objc private func didTapScan() {
        switchToAvatarModeIfAvailable()
        if let renderer = avatarRenderer {
            let demo = AvatarPointCloudBuilder.makeDemoSphere(count: 120_000)
            renderer.setPoints(demo)
        }
    }

    @objc private func didTapTutorial() {
        showAlert(title: "Tutorial", message: "Видео-урок пока недоступен.")
    }

    // MARK: Mode switches

    private func switchToPhotoMode() {
        mode = .photo2D
        metalView.delegate = photoRenderer
        activeRenderer = photoRenderer
        scanButton.isHidden = !isAvatarAvailable
        tutorialButton.isHidden = !isAvatarAvailable
    }

    private var isAvatarAvailable: Bool {
        if #available(iOS 17.0, *) {
            return avatarRenderer != nil
        }
        return false
    }

    private func switchToAvatarModeIfAvailable() {
        guard isAvatarAvailable, let avatarRenderer else {
            showAlert(title: "Недоступно", message: "3D Avatar требует iOS 17+")
            return
        }
        mode = .avatar3D
        metalView.delegate = avatarRenderer
        activeRenderer = avatarRenderer
        scanButton.isHidden = false
        tutorialButton.isHidden = false
        UIView.animate(withDuration: 0.25) {
            self.photoControls.alpha = 0
            self.photoControls.isHidden = true
            self.avatarControls.isHidden = false
            self.avatarControls.alpha = 1
        }
    }

    // MARK: Picker

    private func presentImagePicker(sourceType: UIImagePickerController.SourceType) {
        guard UIImagePickerController.isSourceTypeAvailable(sourceType) else {
            showAlert(title: "Недоступно", message: "Источник недоступен")
            return
        }
        let picker = UIImagePickerController()
        picker.sourceType = sourceType
        picker.allowsEditing = false
        picker.delegate = self
        present(picker, animated: true)
    }

    func imagePickerController(_ picker: UIImagePickerController, didFinishPickingMediaWithInfo info: [UIImagePickerController.InfoKey : Any]) {
        let image = (info[.editedImage] ?? info[.originalImage]) as? UIImage
        if let img = image {
            switchToPhotoMode()
            photoRenderer.setImage(img)
        }
        picker.dismiss(animated: true)
    }

    func imagePickerControllerDidCancel(_ picker: UIImagePickerController) {
        picker.dismiss(animated: true)
    }

    // MARK: Helpers

    private func showAlert(title: String, message: String) {
        let alert = UIAlertController(title: title, message: message, preferredStyle: .alert)
        alert.addAction(UIAlertAction(title: "OK", style: .default))
        present(alert, animated: true)
    }

    private func updateModeButtons() {
        switch mode {
        case .photo2D:
            updateModeButton(modePhotoButton, active: true)
            updateModeButton(modeAvatarButton, active: false)
            photoControls.isHidden = false
            UIView.animate(withDuration: 0.25) {
                self.photoControls.alpha = 1
                self.avatarControls.alpha = 0
            } completion: { _ in
                self.avatarControls.isHidden = true
            }
        case .avatar3D:
            updateModeButton(modePhotoButton, active: false)
            updateModeButton(modeAvatarButton, active: true)
            avatarControls.isHidden = false
            UIView.animate(withDuration: 0.25) {
                self.avatarControls.alpha = 1
                self.photoControls.alpha = 0
            } completion: { _ in
                self.photoControls.isHidden = true
            }
        }
    }
}
