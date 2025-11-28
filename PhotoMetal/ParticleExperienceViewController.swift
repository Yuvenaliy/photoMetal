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
    private let topBlur = UIVisualEffectView(effect: UIBlurEffect(style: .systemThinMaterialDark))
    private let bottomBlur = UIVisualEffectView(effect: UIBlurEffect(style: .systemThinMaterialDark))
    private let modeSegment = UISegmentedControl(items: ["Photo", "3D Avatar"])
    private let titleLabel = UILabel()
    private let subtitleLabel = UILabel()
    private let slider = UISlider()
    private let sliderValueLabel = UILabel()
    private let actionPhotoButton = UIButton(type: .system)
    private let actionCameraButton = UIButton(type: .system)
    private let actionShareButton = UIButton(type: .system)
    private let triggerButton = UIButton(type: .system)
    private let scanButton = UIButton(type: .system)
    private let tutorialButton = UIButton(type: .system)

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
        // Top blur bar
        topBlur.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(topBlur)
        NSLayoutConstraint.activate([
            topBlur.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            topBlur.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            topBlur.topAnchor.constraint(equalTo: view.topAnchor),
            topBlur.heightAnchor.constraint(equalToConstant: 120)
        ])

        let brandButton = UIButton(type: .system)
        brandButton.setImage(UIImage(systemName: "aperture"), for: .normal)
        brandButton.tintColor = .white
        brandButton.translatesAutoresizingMaskIntoConstraints = false

        modeSegment.selectedSegmentIndex = 0
        modeSegment.translatesAutoresizingMaskIntoConstraints = false
        modeSegment.addTarget(self, action: #selector(modeChanged), for: .valueChanged)

        let moreButton = UIButton(type: .system)
        moreButton.setImage(UIImage(systemName: "ellipsis"), for: .normal)
        moreButton.tintColor = .white
        moreButton.translatesAutoresizingMaskIntoConstraints = false

        let topStack = UIStackView(arrangedSubviews: [brandButton, modeSegment, moreButton])
        topStack.axis = .horizontal
        topStack.alignment = .center
        topStack.spacing = 12
        topStack.translatesAutoresizingMaskIntoConstraints = false
        topBlur.contentView.addSubview(topStack)

        NSLayoutConstraint.activate([
            brandButton.widthAnchor.constraint(equalToConstant: 44),
            brandButton.heightAnchor.constraint(equalToConstant: 44),
            moreButton.widthAnchor.constraint(equalToConstant: 44),
            moreButton.heightAnchor.constraint(equalToConstant: 44),
            topStack.leadingAnchor.constraint(equalTo: topBlur.contentView.leadingAnchor, constant: 16),
            topStack.trailingAnchor.constraint(equalTo: topBlur.contentView.trailingAnchor, constant: -16),
            topStack.bottomAnchor.constraint(equalTo: topBlur.contentView.bottomAnchor, constant: -16),
            topStack.topAnchor.constraint(equalTo: topBlur.contentView.topAnchor, constant: 44)
        ])

        // Center labels
        titleLabel.text = "Metal Reality Kit"
        titleLabel.font = UIFont.preferredFont(forTextStyle: .title2)
        titleLabel.textColor = .white
        titleLabel.translatesAutoresizingMaskIntoConstraints = false

        subtitleLabel.text = "Curl Noise • Radial Particles"
        subtitleLabel.font = UIFont.preferredFont(forTextStyle: .subheadline)
        subtitleLabel.textColor = .secondaryLabel
        subtitleLabel.translatesAutoresizingMaskIntoConstraints = false

        let centerStack = UIStackView(arrangedSubviews: [titleLabel, subtitleLabel])
        centerStack.axis = .vertical
        centerStack.alignment = .center
        centerStack.spacing = 4
        centerStack.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(centerStack)
        NSLayoutConstraint.activate([
            centerStack.centerXAnchor.constraint(equalTo: view.centerXAnchor),
            centerStack.centerYAnchor.constraint(equalTo: view.centerYAnchor, constant: -80)
        ])

        // Bottom controls
        bottomBlur.translatesAutoresizingMaskIntoConstraints = false
        view.addSubview(bottomBlur)
        NSLayoutConstraint.activate([
            bottomBlur.leadingAnchor.constraint(equalTo: view.leadingAnchor),
            bottomBlur.trailingAnchor.constraint(equalTo: view.trailingAnchor),
            bottomBlur.bottomAnchor.constraint(equalTo: view.bottomAnchor),
            bottomBlur.heightAnchor.constraint(equalToConstant: 220)
        ])

        // Slider
        slider.minimumValue = 0.5
        slider.maximumValue = 3.0
        slider.value = parameters.explosionStrength
        slider.addTarget(self, action: #selector(sliderChanged), for: .valueChanged)
        slider.translatesAutoresizingMaskIntoConstraints = false
        sliderValueLabel.font = UIFont.monospacedDigitSystemFont(ofSize: 12, weight: .medium)
        sliderValueLabel.textColor = .secondaryLabel
        sliderValueLabel.text = String(format: "%.2f", slider.value)
        sliderValueLabel.translatesAutoresizingMaskIntoConstraints = false

        let sliderStack = UIStackView(arrangedSubviews: [UIImageView(image: UIImage(systemName: "sparkles")), slider, sliderValueLabel])
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

        let bottomStack = UIStackView(arrangedSubviews: [sliderStack, actionRow, scanStack, tutorialStack])
        bottomStack.axis = .vertical
        bottomStack.spacing = 16
        bottomStack.alignment = .center
        bottomStack.translatesAutoresizingMaskIntoConstraints = false
        bottomBlur.contentView.addSubview(bottomStack)
        NSLayoutConstraint.activate([
            bottomStack.leadingAnchor.constraint(equalTo: bottomBlur.contentView.leadingAnchor, constant: 16),
            bottomStack.trailingAnchor.constraint(equalTo: bottomBlur.contentView.trailingAnchor, constant: -16),
            bottomStack.topAnchor.constraint(equalTo: bottomBlur.contentView.topAnchor, constant: 16),
            bottomStack.bottomAnchor.constraint(equalTo: bottomBlur.contentView.bottomAnchor, constant: -16),
            sliderStack.widthAnchor.constraint(equalTo: bottomStack.widthAnchor),
            actionRow.widthAnchor.constraint(equalTo: bottomStack.widthAnchor, multiplier: 0.9)
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

    // MARK: Actions

    @objc private func modeChanged() {
        if modeSegment.selectedSegmentIndex == 0 {
            switchToPhotoMode()
        } else {
            switchToAvatarModeIfAvailable()
        }
    }

    @objc private func sliderChanged() {
        parameters.explosionStrength = slider.value
        sliderValueLabel.text = String(format: "%.2f", slider.value)
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
            modeSegment.selectedSegmentIndex = 0
            return
        }
        mode = .avatar3D
        metalView.delegate = avatarRenderer
        activeRenderer = avatarRenderer
        scanButton.isHidden = false
        tutorialButton.isHidden = false
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
            modeSegment.selectedSegmentIndex = 0
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
}
